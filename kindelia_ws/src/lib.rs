use futures_util::{SinkExt, StreamExt};
use serde::Serialize;
use tokio::sync::broadcast;
use warp::ws::{Message, WebSocket};
use warp::{Filter, Rejection, Reply};

use std::convert::Infallible;
use std::str::FromStr;

#[derive(serde::Deserialize)]
struct QueryParams {
  tags: Option<String>,
}

pub struct Query {
  tags: Vec<String>,
}

/// As the main function of this crate, this will define a Warp filter that
/// handles all the connections to the WebSocket events API.
///
/// This websocket endpoint will share with all of its clients (broadcast) the
/// items comming from the `ws_tx` channel.
pub fn ws_router<T, D>(
  ws_tx: broadcast::Sender<T>,
) -> impl warp::Filter<Extract = (impl Reply,), Error = Rejection> + Clone + Sized
where
  T: Send + Clone + Serialize + 'static,
  D: Send + From<T> + FromStr<Err = String> + Eq + 'static,
{
    warp::ws()
    .and(with_rx(ws_tx))
    .and(warp::query::<QueryParams>().map(parse_query))
    .and_then(ws_handler::<T, D>)
}

fn parse_query(query: QueryParams) -> Query {
  let tags = match query.tags {
    Some(tags) => tags.split(',').map(str::to_string).collect(),
    None => vec![],
  };
  Query { tags }
}

fn with_rx<T>(
  ws_tx: broadcast::Sender<T>,
) -> impl Filter<Extract = (broadcast::Sender<T>,), Error = Infallible> + Clone
where
  T: Send + Clone + Serialize,
{
  warp::any().map(move || ws_tx.clone())
}

pub async fn ws_handler<T, D>(
  ws: warp::ws::Ws,
  ws_tx: broadcast::Sender<T>,
  query: Query,
) -> Result<impl Reply, Rejection>
where
  T: Send + Clone + Serialize + 'static,
  D: Send + From<T> + FromStr<Err = String> + Eq + 'static,
{
  Ok(ws.on_upgrade(move |socket| {
    client_connection::<T, D>(socket, ws_tx, query.tags)
  }))
}

async fn client_connection<T, D>(
  ws: WebSocket,
  ws_tx: broadcast::Sender<T>,
  tags: Vec<String>,
) where
  T: Send + Clone + Serialize + 'static,
  D: Send + From<T> + FromStr<Err = String> + Eq,
{
  let (mut client_ws_sender, _) = ws.split();
  let mut ws_rx = ws_tx.subscribe();
  let mut count = 0;

  while let Ok(event) = ws_rx.recv().await {
    let tags: Result<Vec<D>, String> =
      tags.iter().map(|tag| D::from_str(tag)).collect();

    if let Ok(tags) = tags {
      if tags.is_empty() || tags.contains(&(event.clone().into())) {
        let json_stringfied = serde_json::to_string(&event).unwrap();
        if let Err(err) =
          client_ws_sender.send(Message::text(json_stringfied)).await
        {
          eprintln!("Could not send message through websocket: {}", err);
          count += 1;
        } else {
          count = 0;
        };
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
