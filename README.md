# chain-hash-hs

Simulates a data stream with checksums that apply to each item in the stream.

If you would like to know more, check out the [problem statement](https://github.com/jbristow/chain-hash/blob/master/doc/chain_hash.md).


## Run

* Download the binary `chain-hash-hs-exe`

### Encode

```bash
chain-hash-hs-exe encode -i <input-file> -o <encoded-file>
```
This creates a file filled with checksums at the location specified by `--output`

The hash of the first block will be printed on stdout.

### Decode
```bash
chain-hash-hs-exe decode -i <encoded-file> -o <decoded-file> -c <checksum>
```
Takes a checksum "encoded" file and cleans it piece by piece.  If anything has
been modified, the chunk that contains the beginning of the modification will
be marked invalid and the program will throw an error denoting the offending
piece.

## Compiling

* Install [haskell-stack](https://docs.haskellstack.org/en/stable/README/)
* Run `stack build` to generate the binary. (First run will take a long time,
  `stack` needs to download a large chunk of the haskell-verse and make a local
  copy for this project.

## Notes

* This was done in a couple hours using the clojure code as a base. Most time
  was spent debugging an error because `scanr` is different from `reductions`.
* Gotta love the static-linked binary and the more robust argparse library.
* No tests because I was mainly seeing how long this took me.
