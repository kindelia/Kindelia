#![allow(dead_code)]

use std::fmt::Debug;
use std::ops::Deref;

use reqwest::{Client, IntoUrl, Method, RequestBuilder, Url};
use serde::{de::DeserializeOwned, Serialize};

use crate::{hvm::{Term, self}, node};

use super::{BlockInfo, FuncInfo, Hash, Name, Stats, HexStatement, CtrInfo};

pub struct ApiClient {
  client: reqwest::Client,
  base_url: Url,
}

impl Deref for ApiClient {
  type Target = Url;
  fn deref(&self) -> &Url {
    &self.base_url
  }
}

type ApiResult<T> = Result<T, String>;

impl ApiClient {
  pub fn new<U>(
    base_url: U,
    client: Option<Client>,
  ) -> Result<Self, reqwest::Error>
  where
    U: IntoUrl + Debug,
  {
    let url_txt = format!("{:?}", base_url);
    let base_url = base_url
      .into_url()
      .unwrap_or_else(|_| panic!("Invalid base URL: '{}'.", url_txt));
    let client = client.unwrap_or_else(Client::new);
    Ok(ApiClient { client, base_url })
  }

  pub fn base_request(&self, method: Method, path: &str) -> RequestBuilder {
    let url = self
      .base_url
      .join(path)
      .unwrap_or_else(|err| panic!("Invalid URL sub-path '{}'; {}", path, err));
    self.client.request(method, url)
  }

  pub fn base_get(&self, path: &str) -> RequestBuilder {
    self.base_request(Method::GET, path)
  }

  pub async fn req<T, B>(
    &self,
    method: Method,
    path: &str,
    body: Option<B>,
  ) -> Result<T, String>
  where
    T: DeserializeOwned,
    B: Serialize,
  {
    let req = self.base_request(method, path);
    let req = if let Some(body) = body {
      req.json(&body)
    } else {
      req
    };
    let res = req.send().await.map_err(|e| e.to_string())?;
    if res.status().is_success() {
      let value = res.json().await.map_err(|e| e.to_string())?;
      Ok(value)
    } else {
      let status = res.status();
      let text = res.text().await.map_err(|e| e.to_string())?;
      Err(format!("Error {}: {}", status, text)) // TODO: better messages
    }
  }

  pub async fn get<T>(&self, path: &str) -> ApiResult<T>
  where
    T: DeserializeOwned,
  {
    self.req::<T, String>(Method::GET, path, None).await
  }

  pub async fn get_stats(&self) -> ApiResult<Stats> {
    self.get::<Stats>("/stats").await
  }

  pub async fn get_blocks(&self) -> ApiResult<Vec<BlockInfo>> {
    self.get::<Vec<BlockInfo>>("/blocks").await
  }

  pub async fn get_block(&self, id: Hash) -> ApiResult<BlockInfo> {
    self.get::<BlockInfo>(&format!("/blocks/{}", id)).await
  }

  pub async fn get_functions(&self) -> ApiResult<Vec<Name>> {
    self.get::<Vec<Name>>("/functions").await
  }

  pub async fn get_function(&self, name: Name) -> ApiResult<FuncInfo> {
    self.get::<FuncInfo>(&format!("/functions/{}", name)).await
  }

  pub async fn get_function_state(&self, name: Name) -> ApiResult<Term> {
    self.get::<Term>(&format!("/functions/{}/state", name)).await
  }

  pub async fn get_constructor(&self, name: Name) -> ApiResult<CtrInfo> {
    self.get::<CtrInfo>(&format!("/constructor/{}", name)).await
  }

  pub async fn run_code(&self, code: Vec<HexStatement>) -> ApiResult<Vec<hvm::StatementInfo>> {
    self.req(Method::POST, "/run", Some(code)).await
  }

  // I'm not sure what the return type should be.
  pub async fn publish_code(&self, code: Vec<HexStatement>) -> ApiResult<Vec<Result<(), ()>>> {
    self.req(Method::POST, "/publish", Some(code)).await
  }

  pub async fn get_peers(&self, all: bool) -> ApiResult<Vec<node::Peer>> {
    if all {
      self.get::<Vec<node::Peer>>("/peers/all").await
    } else {
      self.get::<Vec<node::Peer>>("/peers").await
    }
  }
}
