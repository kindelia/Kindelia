use super::Stats;

pub async fn stats() -> Result<Stats, reqwest::Error> {
  let base = "http://localhost:8000";
  let url = format!("{}/tick", base);
  let res = reqwest::Client::new().get(url).send().await?;
  let stats: Stats = res.json().await?;
  Ok(stats)
}
