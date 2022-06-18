extern crate secp256k1;

use secp256k1::rand::rngs::OsRng;
use secp256k1::ecdsa::{RecoverableSignature, RecoveryId};
use secp256k1::{Secp256k1, Message, SecretKey, PublicKey};
use tiny_keccak::Hasher;

#[derive(Debug)]
pub struct Signature(pub [u8; 65]);
pub struct Address(pub [u8; 20]);
pub struct Hash(pub [u8; 32]);
pub struct Name(pub u128);

pub fn keccak256(data: &[u8]) -> Hash {
  let mut hasher = tiny_keccak::Keccak::v256();
  let mut output = [0u8; 32];
  hasher.update(&data);
  hasher.finalize(&mut output);
  return Hash(output);
}

pub struct Account {
  secret_key: SecretKey,
  pub public_key: PublicKey,
  pub address: Address,
  pub name: Name,
}

impl Signature {
  pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
    Some(Signature(bytes.try_into().ok()?))
  }

  pub fn from_hex(hex: &str) -> Option<Self> {
    Signature::from_bytes(hex::decode(hex).ok()?.as_slice())
  }

  pub fn to_hex(&self) -> String {
    hex::encode(self.0)
  }

  pub fn signer_public_key(&self, hash: &Hash) -> Option<PublicKey> {
    let recovery_id = RecoveryId::from_i32(self.0[0] as i32).ok()?;
    let sign_data = self.0[1..65].try_into().unwrap();
    let signature = RecoverableSignature::from_compact(sign_data, recovery_id).ok()?;
    return signature.recover(&Message::from_slice(&hash.0).expect("32 bytes hash")).ok();
  }

  pub fn signer_address(&self, hash: &Hash) -> Option<Address> {
    return Some(Address::from_public_key(&self.signer_public_key(hash)?));
  }

  pub fn signer_name(&self, hash: &Hash) -> Option<Name> {
    return Some(Name::from_public_key(&self.signer_public_key(hash)?));
  }
}

impl Address {
  pub fn from_public_key(pubk: &PublicKey) -> Self {
    return Address::from_hash(&Account::hash_public_key(pubk));
  }

  pub fn from_hash(hash: &Hash) -> Self {
    return Address(hash.0[12..32].try_into().unwrap());
  }
}

impl Name {
  pub fn from_public_key(pubk: &PublicKey) -> Self {
    return Name::from_hash(&Account::hash_public_key(pubk));
  }

  // A Kindelia name is the first 120 bits of an Ethereum address
  // This corresponds to the bytes 12-27 of the ECDSA public key.
  pub fn from_hash(hash: &Hash) -> Self {
    return Name(u128::from_be_bytes(vec![hash.0[12..27].to_vec(), vec![0]].concat().try_into().unwrap()) >> 8);
  }
}

impl Account {
  pub fn new() -> Account {
    return Account::from_secret_key(SecretKey::new(&mut OsRng::new().expect("OsRng")));
  }

  pub fn hash_public_key(pubk: &PublicKey) -> Hash {
    return keccak256(&pubk.serialize_uncompressed()[1..65].to_vec());
  }

  pub fn from_private_key(key: &[u8]) -> Self {
    Account::from_secret_key(SecretKey::from_slice(&key).expect("32 bytes private key"))
  }

  pub fn from_secret_key(secret_key: SecretKey) -> Self {
    let pubk = PublicKey::from_secret_key(&Secp256k1::new(), &secret_key);
    let hash = Account::hash_public_key(&pubk);
    let addr = Address::from_hash(&hash);
    let name = Name::from_hash(&hash);
    return Account { secret_key, public_key: pubk, address: addr, name };
  }

  pub fn sign(&self, hash: &Hash) -> Signature {
    let secp = Secp256k1::new();
    let sign = secp.sign_ecdsa_recoverable(&Message::from_slice(&hash.0).expect("32 bytes hash"), &self.secret_key).serialize_compact();
    return Signature([vec![sign.0.to_i32() as u8], sign.1.to_vec()].concat().try_into().unwrap());
  }
}

fn main() {

  // Creates an account from a private key
  let private = hex::decode("0000000000000000000000000000000000000000000000000000000000000001").unwrap();
  let account = Account::from_private_key(&private);
  println!("addr: {}", hex::encode(account.address.0));

  // A message to sign
  let hash = keccak256(b"Hello!");

  // The signature
  let sign = account.sign(&hash);
  println!("sign: {}", hex::encode(sign.0));

  // Recovers the signer
  let auth = sign.signer_address(&hash).unwrap();
  println!("addr: {}", hex::encode(auth.0));

  // The signature, again
  let sign = Signature::from_hex("00d0bd2749ab84ce3851b4a28dd7f3b3e5a51ba6c38f36ef6e35fd0bd01c4a9d3418af687271eff0a37ed95e6a202f5d4efdb8663b361f301d899b3e5596313245").unwrap();
  let auth = sign.signer_address(&hash).unwrap();
  println!("addr: {}", hex::encode(auth.0));
}
