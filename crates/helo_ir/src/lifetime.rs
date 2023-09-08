use bitvec::prelude as bv;

type BitVec = bv::BitVec<u32, bv::Msb0>;

struct LifetimeStore {
    v: BitVec,
}
