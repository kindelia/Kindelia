use futures_util::{SinkExt, StreamExt};
use tokio::sync::broadcast;
use warp::ws::{Message, WebSocket};
use warp::{Filter, Rejection, Reply};

use kindelia_core::events::{NodeEventDiscriminant, NodeEventType};

use std::convert::Infallible;
use std::str::FromStr;

#[derive(serde::Deserialize)]
struct QueryParams {
  tags: Option<String>,
}

struct Query {
  tags: Vec<String>,
}

/// As the main function of this crate, this will define a Warp filter that
/// handles all the connections to the WebSocket events API.
///
/// This websocket endpoint will share with all of its clients (broadcast) the
/// items comming from the `ws_tx` channel.
pub fn ws_router(
  ws_tx: broadcast::Sender<NodeEventType>,
) -> impl warp::Filter<Extract = (impl Reply,), Error = Rejection> + Clone + Sized
{
  warp::ws()
    .and(with_rx(ws_tx))
    .and(warp::query::<QueryParams>().map(parse_query))
    .and_then(ws_handler)
}

fn parse_query(query: QueryParams) -> Query {
  let tags = match query.tags {
    Some(tags) => tags.split(',').map(str::to_string).collect(),
    None => vec![],
  };
  Query { tags }
}

fn with_rx(
  ws_tx: broadcast::Sender<NodeEventType>,
) -> impl Filter<Extract = (broadcast::Sender<NodeEventType>,), Error = Infallible>
     + Clone {
  warp::any().map(move || ws_tx.clone())
}

async fn ws_handler(
  ws: warp::ws::Ws,
  ws_tx: broadcast::Sender<NodeEventType>,
  query: Query,
) -> Result<impl Reply, Rejection> {
  Ok(ws.on_upgrade(move |socket| client_connection(socket, ws_tx, query.tags)))
}

async fn client_connection(
  ws: WebSocket,
  ws_tx: broadcast::Sender<NodeEventType>,
  tags: Vec<String>,
) {
  let (mut client_ws_sender, _) = ws.split();
  let mut ws_rx = ws_tx.subscribe();
  let mut count = 0;

  while let Ok(event) = ws_rx.recv().await {
    let tags: Result<Vec<NodeEventDiscriminant>, String> =
      tags.iter().map(|tag| NodeEventDiscriminant::from_str(tag)).collect();

    if let Ok(tags) = tags {
      if tags.is_empty() || tags.contains(&(event.clone().into())) {
        match serde_json::to_string(&event) {
          Err(err) => {
            eprintln!("Could not convert {} into json: {}", event, err)
          }
          Ok(json_stringfied) => {
            match client_ws_sender.send(Message::text(json_stringfied)).await {
              Err(err) => {
                eprintln!("Could not send message through websocket: {}", err);
                count += 1;
              }
              Ok(_) => count = 0,
            }
          }
        }

        // After 10 consecutive fails we close the connection
        if count == 10 {
          break;
        };
      }
    } else {
      break;
    }
  }

  eprintln!("Disconnected");
}
