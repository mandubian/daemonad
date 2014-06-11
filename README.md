# Welcome to Daemonad

> THIS PROJECT IS VERY DRAFT AND NOT STABLE AND NOT MANAGING ALL KINDS OF CODE YET

A categorical programming facility for Scala that offers a direct API for working with monad & a few monad stacks (at least trying).


## TODO

- rely on `MonadTrans[F[_], _]` instead of hardcoding monad transformers.
- accept custom `MonadTrans` provided in the user code.