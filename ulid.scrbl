#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     ulid))

@title{@tt{ulid}: universally unique lexicographically sortable identifiers}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[ulid]

This package provides an implementation of ULIDs, which are a
lexicographically-sortable alternative to UUIDs.

@defthing[ulid/c string?]{
  Represents a ULID string.
}

@defthing[ulid-bytes/c bytes?]{
  Represents the binary representation of a ULID.
}

@defproc[(make-ulid-factory) (-> (and/c ulid/c immutable?))]{
  Returns a function that can be used to generate ULIDs.  Any ULIDs
  generated within the same millisecond by the resulting function will
  increase monotonically.

  The generator functions are thread-safe.
}

@defproc[(ulid) (-> (and/c ulid/c immutable?))]{
  Generates an ULID.
}

@defproc[(ulid-time [s ulid/c]) exact-nonnegative-integer?]{
  Returns the time component of the ULID @racket[s].
}

@defproc[(ulid-randomness [s ulid/c]) exact-nonnegative-integer?]{
  Returns the random component of the ULID @racket[s].
}

@defproc[(ulid->bytes [u ulid/c]) ulid-bytes/c]{
  Returns the binary representation of the ULID @racket[u].
}

@defproc[(bytes->ulid [bs ulid-bytes/c]) ulid/c]{
  Converts @racket[bs] to a ULID.  Raises an error if the result is
  not a valid ULID.
}
