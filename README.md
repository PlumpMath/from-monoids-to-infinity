# From Monoids to Infinity
Custom made Haskell types with monoid, functor, applicatives, monads, foldables, traversables and beyond

## Recap - Laws

### Monoids (mappend, mempty)

1. Left identity

`mappend mempty x = x`

2. Right identity

`mappend x mempty = x`

3. Associativity

`mappend x (mappend y z) = (mappend x y) mappend z`

### Functors (fmap)

1. Identity

`fmap id = id`

2. Composition

`fmap (f . g) == fmap f . fmap g`

### Applicatives (pure, <*>)

1. Identity

`pure id <*> v = v`

2. Composition

`pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`

3. Homomorphism

`pure f <*> pure x = pure (f x)`

4. Interchange

`u <*> pure y = pure ($ y) <*> u`

### Monads (>>=, return)

1. Left identity

`return v >>= m = m v`

2. Right identity

`m >>= return = m`

3. Associativity

`(m >>= f) >>= g = m >>= (\x -> f x >>= g)`