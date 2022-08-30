#![allow(dead_code)]

use std::fmt::Debug;
use std::ops::Deref;

use reqwest::{Client, IntoUrl, Method, RequestBuilder, Url};
use serde::{de::DeserializeOwned, Serialize};

use crate::hvm::Term;

use super::{BlockInfo, FuncInfo, Hash, Name, Stats, CountStats};

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

type ApiResult<T> = Result<T, reqwest::Error>;

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
      .unwrap_or_else(|_| panic!("Invalid URL sub-path '{}'.", path));
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
  ) -> Result<T, reqwest::Error>
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
    let res = req.send().await?;
    let value = res.json().await?;
    Ok(value)
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

  pub async fn count_stats(&self) -> ApiResult<CountStats> {
    self.get::<CountStats>("/count").await
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

  pub async fn run_code(&self, code: &str) -> ApiResult<String> {
    self.req(Method::POST, "/run", Some(code)).await
  }

  pub async fn post_code(&self, code: &str) -> ApiResult<String> {
    self.req(Method::POST, "/post", Some(code)).await
  }
}
