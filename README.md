# applicatives
Haskell applicatives with QuickCheck testing. Just that =)

## Recap - Laws

### Monoids

1. Left identity
`mappend mempty x = x`

2. Right identity
`mappend x mempty = x`

3. Associativity
`mappend x (mappend y z) = (mappend x y) mappend z`

### Functors

1. Identity
`fmap id = id`

2. Composition
`fmap (f . g) == fmap f . fmap g`

### Applicatives

1. Identity
`pure id <*> v = v`

2. Composition
`pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`

3. Homomorphism
`pure f <*> pure x = pure (f x)`

4. Interchange
`u <*> pure y = pure ($ y) <*> u`

### Monads

1. Left identity
`return v >>= m = m v`

2. Right identity
`m >>= return = m`

3. Associativity
`(m >>= f) >>= g = m >>= (\x -> f x >>= g)`