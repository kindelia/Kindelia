use std::io::{Read, Write, Result as IoResult, Error, ErrorKind};
use std::hash::{Hash, BuildHasher};
use std::collections::HashMap;
use std::sync::Arc;
use std::ops::Deref;
use crate::hvm::{CompFunc, Func, compile_func};
use crate::bits::ProtoSerialize;


/// Trait that represents serialization of a type to memory.
/// `disk_serialize` expects a sink to write to and returns the amount of bytes written
/// `disk_deserialize` expects a source to read from, and returns an option:
///  - Some(obj) represents that it was successfully created.
///  - None represents that the `source` was empty.
pub trait DiskSer
where
  Self: Sized,
{
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize>;
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<Self>>;
}

impl DiskSer for u8 {
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize> {
    sink.write(&self.to_le_bytes())
  }
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<u8>> {
    let mut buf = [0; 1];
    let bytes_read = source.read(&mut buf)?;
    match bytes_read {
      0 => { Ok(None) }
      _ => { Ok(Some(u8::from_le_bytes(buf))) }
    }
  }
}

// All numeric serializations are just this `u128` boilerplate
// We could write this for any Type that implements
// the function `from_le_bytes`.
impl DiskSer for u128 {
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize> {
    sink.write(&self.to_le_bytes())
  }
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<u128>> {
    const BYTES : usize = (u128::BITS / 8) as usize;
    const AT_MOST : usize = BYTES-1;
    let mut buf = [0; BYTES];
    let bytes_read = source.read(&mut buf)?;
    match bytes_read {
      0 => { Ok(None) }
      1..=AT_MOST => { Err(Error::from(ErrorKind::UnexpectedEof)) }
      _ => { Ok(Some(u128::from_le_bytes(buf))) }
    }
  }
}

impl DiskSer for u64 {
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize> {
    sink.write(&self.to_le_bytes())
  }
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<u64>> {
    const BYTES : usize = (u64::BITS / 8) as usize;
    const AT_MOST : usize = BYTES-1;
    let mut buf = [0; BYTES];
    let bytes_read = source.read(&mut buf)?;
    match bytes_read {
      0 => { Ok(None) }
      1..=AT_MOST => { Err(Error::from(ErrorKind::UnexpectedEof)) }
      _ => { Ok(Some(u64::from_le_bytes(buf))) }
    }
  }
}

// We assume that every map will be stored in a whole file.
// because of that, it will consume all of the file while reading it.
impl<K, V, H> DiskSer for HashMap<K, V, H>
where
  K: DiskSer + Eq + Hash,
  V: DiskSer,
  H: BuildHasher + Default,
{
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize> {
    let mut total_written = 0;
    for (k, v) in self {
      let key_size = k.disk_serialize(sink)?;
      let val_size = v.disk_serialize(sink)?;
      total_written += key_size + val_size;
    }
    Ok(total_written)
  }
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<Self>> {
    let mut slf = HashMap::with_hasher(H::default());
    while let Some(key) = K::disk_deserialize(source)? {
      let val = V::disk_deserialize(source)?;
      if let Some(val) = val {
        slf.insert(key, val);
      }
      else {
        return Err(Error::from(ErrorKind::UnexpectedEof));
      }     
    }
    Ok(Some(slf))
  }
}

impl <K> DiskSer for Vec<K>
where
  K: DiskSer,
{
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize> {
    let mut total_written = 0;
    for elem in self {
      let elem_size = elem.disk_serialize(sink)?;
      total_written += elem_size;
    }
    Ok(total_written)      
  }
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<Self>> {
    let mut res = Vec::new();
    while let Some(elem) = K::disk_deserialize(source)? {
        res.push(elem);
    }
    Ok(Some(res))
  }
}

impl<T> DiskSer for Arc<T>
where
  T: DiskSer,
{
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize> {
    let t = Arc::deref(self);
    t.disk_serialize(sink)
  }
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<Self>> {
    let t = T::disk_deserialize(source)?;
    Ok(t.map(Arc::new))
  }
}

impl DiskSer for CompFunc {
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize>{
    let func_buff = self.func.proto_serialized().to_bytes();
    let size = func_buff.len() as u128;
    let written1 = size.disk_serialize(sink)?;
    let written2 = func_buff.disk_serialize(sink)?;
    Ok(written1 + written2)
  }
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<Self>> {
    // let compfunc = CompFunc {};
    if let Some(len) = u128::disk_deserialize(source)? {
      let len = len as usize;
      let mut buf = vec![0; len];
      let read_bytes = source.read(&mut buf)?;
      if read_bytes != len {
        return Err(Error::from(ErrorKind::UnexpectedEof));
      }
      let func = &Func::proto_deserialized(&bit_vec::BitVec::from_bytes(&buf))
        .ok_or_else(|| Error::from(ErrorKind::InvalidData))?; // invalid data? which error is better?
      let func = compile_func(func, false)
        .map_err(|_| Error::from(ErrorKind::InvalidData))?; // TODO: return error in deserialization? 
      Ok(Some(func))
    }
    else {
      Ok(None)
    }
  }
} 

impl<T: DiskSer + Default + std::marker::Copy, const N: usize> DiskSer for [T; N]
{
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize> {
    let mut total_written = 0;
    for elem in self {
      let elem_size = elem.disk_serialize(sink)?;
      total_written += elem_size;
    }
    Ok(total_written)      
  }
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<Self>> {
    let mut res: [T; N] = [T::default(); N];
    for (i, e) in res.iter_mut().take(N).enumerate() {
      let read = T::disk_deserialize(source)?;
      match (i, read) {
        (_, Some(elem)) => *e = elem,
        (0, None) => return Ok(None),
        (_, None) => return Err(Error::from(ErrorKind::UnexpectedEof)),
      }
    }
    Ok(Some(res))
  }
}

impl DiskSer for crate::crypto::Hash {
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize>{ 
    self.0.disk_serialize(sink)
  }
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<Self>> {
    let hash = <[u8; 32]>::disk_deserialize(source)?;
    Ok(hash.map(crate::crypto::Hash))
  }
}


impl DiskSer for crate::hvm::RawCell {
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize>{
    (**self).disk_serialize(sink)
  }
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<Self>> {
    let cell = u128::disk_deserialize(source)?;
    match cell {
      None => Ok(None),
      Some(num) => {
        let rawcell = crate::hvm::RawCell::new(num);
        match rawcell {
          Some(rawcell) => Ok(Some(rawcell)),
          None => Err(Error::from(ErrorKind::InvalidData))
        }
      }
    }
  }
}

impl DiskSer for crate::hvm::Loc {
  fn disk_serialize<W: Write>(&self, sink: &mut W) -> IoResult<usize>{ 
    (**self).disk_serialize(sink)
  }
  fn disk_deserialize<R: Read>(source: &mut R) -> IoResult<Option<Self>> {
    let loc = u64::disk_deserialize(source)?;
    match loc {
      None => Ok(None),
      Some(num) => {
        let loc = crate::hvm::Loc::new(num);
        match loc {
          Some(loc) => Ok(Some(loc)),
          None => Err(Error::from(ErrorKind::InvalidData))
        }
      }
    }
  }
}

