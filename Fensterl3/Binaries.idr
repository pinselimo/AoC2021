module Fensterl3.Binaries

import Data.Nat

public export
data Bin : Type where
  Z : Bin
  B0 : Bin -> Bin
  B1 : Bin -> Bin

bin_incr : Bin -> Bin
bin_incr Z      = B1 Z
bin_incr (B0 x) = B1 x
bin_incr (B1 x) = B0 (bin_incr x)

export
binToNat : Bin -> Nat
binToNat Z = 0
binToNat (B1 x) = 1 + (binToNat x + binToNat x)
binToNat (B0 x) = binToNat x + binToNat x

export
natToBin : Nat -> Bin
natToBin 0 = Z
natToBin (S n) = bin_incr (natToBin n)

export
normalize : Bin -> Bin
normalize Z = Z
normalize (B1 b) = B1 (normalize b)
normalize (B0 b) = case normalize b of
                        Z => Z
                        x => B0 x

-- Theorems
total
bin_and_nat_incr : (b : Bin) -> S (binToNat b) = binToNat (bin_incr b)
bin_and_nat_incr Z = Refl
bin_and_nat_incr (B1 x) = let
  rec = bin_and_nat_incr x
  pSrS = plusSuccRightSucc (binToNat x) (binToNat x)
  in
  rewrite sym rec in
  rewrite sym pSrS in Refl
bin_and_nat_incr (B0 x) = Refl

total
nat_bin_nat : (n : Nat) -> n = binToNat (natToBin n)
nat_bin_nat Z = Refl
nat_bin_nat (S k) = let
  Hincr = bin_and_nat_incr (natToBin k)
  Hind = nat_bin_nat k
  in rewrite sym Hincr in
     rewrite sym Hind in Refl

total
natDouble_B0 : (n : Nat) -> natToBin (n + n) = case n of Z => Z; _ => B0 (natToBin n)
natDouble_B0 Z = Refl
natDouble_B0 (S k) = let
  IH = natDouble_B0 k
  HpSrS = plusSuccRightSucc k k

  in rewrite sym HpSrS in
     rewrite IH in case k of
       Z => Refl
       (S k') => Refl

total
bin_nat_bin_help1 : (b: Bin) -> B1 (natToBin (binToNat b)) = bin_incr (natToBin (plus (binToNat b) (binToNat b)))
bin_nat_bin_help1 b with (binToNat b)
  bin_nat_bin_help1 b | 0     = Refl
  bin_nat_bin_help1 b | (S k) = let
    HpSrS = plusSuccRightSucc k k
    Hnd = natDouble_B0 k
    in rewrite sym HpSrS in
       rewrite Hnd in case k of
                           0 => Refl
                           (S k') => Refl

total
bin_nat_bin_help2' : Bin -> Bin
bin_nat_bin_help2' b = case natToBin (binToNat b) of
       {Z => Z;
       x => B0 x}

total
bin_nat_bin_help2'' : (k:Nat) -> B0 (bin_incr (natToBin k)) =
                                 case bin_incr (natToBin k) of
                                      {Z => Z;
                                      x => B0 x}
bin_nat_bin_help2'' n with (natToBin n)
  bin_nat_bin_help2'' n | Z = Refl
  bin_nat_bin_help2'' n | (B1 b) = Refl
  bin_nat_bin_help2'' n | (B0 b) = Refl

total
bin_nat_bin_help2 : (b : Bin) -> (bin_nat_bin_help2' b) = (case binToNat b of
                      {0 => Z;
                      n => B0 (natToBin n)})
bin_nat_bin_help2 b with (binToNat b)
  bin_nat_bin_help2 b | 0 = Refl
  bin_nat_bin_help2 b | (S k) = sym (bin_nat_bin_help2'' k)

total
bin_nat_bin : (b : Bin) -> normalize b = natToBin (binToNat b)
bin_nat_bin Z = Refl
bin_nat_bin (B1 b) = rewrite (bin_nat_bin b) in bin_nat_bin_help1 b
bin_nat_bin (B0 b) = let
  IH = bin_nat_bin b
  H2 = bin_nat_bin_help2 b
  H1 = natDouble_B0 (binToNat b) in
  rewrite IH in rewrite H1 in H2
