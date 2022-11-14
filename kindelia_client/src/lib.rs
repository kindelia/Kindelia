#![allow(dead_code)]

use std::ops::Deref;

use serde::{de::DeserializeOwned, Serialize};

use kindelia_core::api::{
  BlockInfo, CtrInfo, FuncInfo, Hash, HexStatement, Name, RegInfo, Stats,
};
use kindelia_core::hvm::{self, Term};
use kindelia_core::net::ProtoComm;
use kindelia_core::node;

pub struct ApiClient {
  client: ureq::Agent,
  base_url: String,
}

impl Deref for ApiClient {
  type Target = String;
  fn deref(&self) -> &String {
    &self.base_url
  }
}

pub enum Method {
  GET,
  POST,
}

impl ToString for Method {
  fn to_string(&self) -> String {
    let res = match self {
      Method::GET => "GET",
      Method::POST => "POST",
    };
    res.to_string()
  }
}

type ApiResult<T> = Result<T, String>;

impl ApiClient {
  /// Receives an `urq::Agent` or build it with default values and
  /// wraps it with the default `base_url` into an `ApiClient` structure.
  pub fn new(base_url: &str, client: Option<ureq::Agent>) -> Self {
    let client = client.unwrap_or_else(|| ureq::AgentBuilder::new().build());
    ApiClient { client, base_url: base_url.to_string() }
  }

  pub fn base_request(&self, method: Method, path: &str) -> ureq::Request {
    let url = format!("{}{}", self.base_url, path);
    self.client.request(&method.to_string(), &url)
  }

  pub fn base_get(&self, path: &str) -> ureq::Request {
    self.base_request(Method::GET, path)
  }

  pub fn req<T, B>(
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
    // {
    //   let req = req.clone();
    //   let res =
    //     if let Some(ref body) = body { req.send_json(body) } else { req.call() };
    //   let res = res.map_err(|e| e.to_string())?;
    //   println!("{}", res.into_string().unwrap());
    // }
    let res =
      if let Some(body) = body { req.send_json(&body) } else { req.call() };
    let res = res.map_err(|e| e.to_string())?;
    if res.status() == 200 {
      let value = res.into_json().map_err(|e| e.to_string())?;
      Ok(value)
    } else {
      let status = res.status();
      let text = res.into_string().map_err(|e| e.to_string())?;
      Err(format!("Error {}: {}", status, text)) // TODO: better messages
    }
  }

  pub async fn get<T>(&self, path: &str) -> ApiResult<T>
  where
    T: DeserializeOwned,
  {
    self.req::<T, String>(Method::GET, path, None)
  }

  pub async fn get_stats(&self) -> ApiResult<Stats> {
    self.get::<Stats>("/stats").await
  }

  pub async fn get_blocks(&self) -> ApiResult<Vec<BlockInfo>> {
    self.get::<Vec<BlockInfo>>("/blocks").await
  }

  pub async fn get_block_hash(&self, index: u64) -> ApiResult<String> {
    self.get::<String>(&format!("/block-hash/{}", index)).await
  }

  pub async fn get_block(&self, id: Hash) -> ApiResult<Option<BlockInfo>> {
    self.get::<Option<BlockInfo>>(&format!("/blocks/{}", id)).await
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

  pub async fn run_code(
    &self,
    code: Vec<HexStatement>,
  ) -> ApiResult<Vec<hvm::StatementInfo>> {
    self.req(Method::POST, "/run", Some(code))
  }

  // I'm not sure what the return type should be.
  pub async fn publish_code(
    &self,
    code: Vec<HexStatement>,
  ) -> ApiResult<Vec<Result<(), ()>>> {
    self.req(Method::POST, "/publish", Some(code))
  }

  pub async fn get_peers<C: ProtoComm>(
    &self,
    all: bool,
  ) -> ApiResult<Vec<node::Peer<C::Address>>>
  where
    C::Address: serde::de::DeserializeOwned,
  {
    if all {
      self.get::<Vec<node::Peer<C::Address>>>("/peers/all").await
    } else {
      self.get::<Vec<node::Peer<C::Address>>>("/peers").await
    }
  }

  pub async fn get_reg_info(&self, name: &str) -> ApiResult<RegInfo> {
    self.get::<RegInfo>(&format!("/reg/{}", name)).await
  }
}
