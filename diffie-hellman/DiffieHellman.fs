module DiffieHellman

open System
open System.Numerics

let modpow a n modulus = BigInteger.ModPow(a,n,modulus)

let rng = new Random()
let privateKey (prime : BigInteger) = 
    rng.Next(1, int prime - 1) //TODO: not really using BigInt primes ey?
    |> BigInteger

let publicKey p g pk = modpow g pk p
let secret p pub priv = modpow pub priv p