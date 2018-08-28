# hhc - simple haskell hash cracker [![CircleCI](https://circleci.com/gh/jjantura/hhc.svg?style=svg)](https://circleci.com/gh/jjantura/hhc)

Main assumptions

The project is meant NOT to be competitive to any other password crackers. It's just a side-product of haskell self-study and can be rather considered as (comparative) benchmark.

Functional TODO:
- other algorithms: MD5, SHA*, RipeMD, ... [done]
- other than brute-force modes (dictionary, dictionary with rules) [done]
- testing [partly done]
- benchmarking single and multithreaded [partly done - single threading]
- hash validation (length, content)
- multithreading
- optimization

Technical TODO:
- replace custom options parsing with optparse-applicative 
- replace custom benchmarking with criterion
