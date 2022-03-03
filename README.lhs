# Scientist

Haskell port of
[`github/scientist`](https://github.com/github/scientist#readme).

## Usage

The following extensions are recommended:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

<!--
```haskell
module Main (module Main) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text)
import UnliftIO.Exception (SomeException, throwIO)
```
-->

Most usage will only require the top-level library:

```haskell
import Scientist
```

<!--
```haskell
import Text.Markdown.Unlit ()

theOriginalCode :: m a
theOriginalCode = undefined

theExperimentalCode :: m a
theExperimentalCode = undefined
```
-->

1. Define a new `Experiment m c a b` with,

   ```haskell
   ex0 :: Functor m => Experiment m c a b
   ex0 = newExperiment "some name" theOriginalCode
   ```

1. The type variables capture the following details:

   - `m`: Some Monad to operate in, e.g. `IO`.
   - `c`: Any context value you attach with `setExperimentContext`. It will then
     be available in the `Result c a b` you publish.
   - `a`: The result type of your original (aka "control") code, this is what is
     always returned and so is the return type of `experimentRun`.
   - `b`: The result type of your experimental (aka "candidate") code. It
     probably won't differ (and must not to use a comparison like `(==)`), but
     it can (provided you implement a comparison between `a` and `b`).

1. Configure the Experiment as desired

   ```haskell
   ex1 :: (Functor m, Eq a) => Experiment m c a a
   ex1 =
     setExperimentPublish publish
       $ setExperimentCompare experimentCompareEq
       $ setExperimentTry theExperimentalCode
       $ newExperiment "some name" theOriginalCode

   -- Increment Statsd, Log, store in Redis, whatever
   publish :: Result c a b -> m ()
   publish = undefined
   ```

1. Run the experiment

   ```haskell
   run0 :: (MonadUnliftIO m, Eq a) => m a
   run0 =
     experimentRun
       $ setExperimentPublish publish
       $ setExperimentCompare experimentCompareEq
       $ setExperimentTry theExperimentalCode
       $ newExperiment "some name" theOriginalCode
   ```

1. Explore things like `setExperimentIgnore`, `setExperimentEnabled`, etc.

---

The rest of this README matches section-by-section to the ported project and
shows only the differences in syntax for those features. Please follow the
header links for additional details, motivation, etc.

## [How do I science?](https://github.com/github/scientist#how-do-i-science)

<!--
```haskell
data Model = Model

data User = User

userLogin :: User -> Text
userLogin = undefined

userCanRead :: User -> Model -> m Bool
userCanRead = undefined

modelCheckUserValid :: Model -> User -> m Bool
modelCheckUserValid = undefined
```
-->

```haskell
myWidgetAllows :: MonadUnliftIO m => Model -> User -> m Bool
myWidgetAllows model user = do
  experimentRun
    $ setExperimentTry
        (userCanRead user model) -- new way
    $ newExperiment "widget-permissions"
        (modelCheckUserValid model user) -- old way
```

## [Making science useful](https://github.com/github/scientist#making-science-useful)

```haskell
run1 :: MonadUnliftIO m => m a
run1 =
  experimentRun
    $ setExperimentEnabled (pure True)
    $ setExperimentOnException onScientistException
    $ setExperimentPublish (liftIO . putStrLn . formatResult)
    $ setExperimentTry theExperimentalCode
    $ newExperiment "some name" theOriginalCode

onScientistException :: MonadIO m => SomeException -> m ()
onScientistException ex = do
  liftIO $ putStrLn "..."

  -- To re-raise
  throwIO ex

formatResult :: Result c a b -> String
formatResult = undefined
```

### [Controlling comparison](https://github.com/github/scientist#controlling-comparison)

<!--
```haskell
userServiceFetch :: m [User]
userServiceFetch = undefined

fetchAllUsers :: m [User]
fetchAllUsers = undefined
```
--->

```haskell
run2 :: MonadUnliftIO m => m [User]
run2 =
  experimentRun
    $ setExperimentCompare (experimentCompareOn $ map userLogin)
    $ setExperimentTry userServiceFetch
    $ newExperiment "users" fetchAllUsers
```

When using `experimentCompareOn`, `By`, or `Eq`, if a candidate branch raises an
exception, that will never compare equally.

### [Adding context](https://github.com/github/scientist#adding-context)

See `setExperimentContext`.

### [Expensive setup](https://github.com/github/scientist#expensive-setup)

Just do it ahead of time.

<!--
```haskell
data X = X

expensiveSetup :: m X
expensiveSetup = undefined

theOriginalCodeWith :: X -> m a
theOriginalCodeWith = undefined

theExperimentalCodeWith :: X -> m a
theExperimentalCodeWith = undefined
```
-->

```haskell
run3 :: MonadUnliftIO m => m a
run3 = do
  x <- expensiveSetup

  experimentRun
    $ setExperimentTry (theExperimentalCodeWith x)
    $ newExperiment "expensive" (theOriginalCodeWith x)
```

### [Keeping it clean](https://github.com/github/scientist#keeping-it-clean)

Not supported at this time. Format the value(s) as necessary when publishing.

### [Ignoring mismatches](https://github.com/github/scientist#ignoring-mismatches)

See `setExperimentIgnore`.

### [Enabling/disabling experiments](https://github.com/github/scientist#enablingdisabling-experiments)

See `setExperimentRunIf`.

### [Ramping up experiments](https://github.com/github/scientist#ramping-up-experiments)

```haskell
run4 :: MonadUnliftIO m => m a
run4 =
  experimentRun
    $ setExperimentEnabled (experimentEnabledPercent 30)
    $ setExperimentTry theExperimentalCode
    $ newExperiment "some name" theOriginalCode
```

### [Publishing results](https://github.com/github/scientist#publishing-results)

```hs
experimentRun
  $ setExperimentPublish publish
  -- ...

publish :: MonadIO m => Results MyContext User User -> m ()
publish result = do
  -- Details are present unless it's a ResultSkipped, which we'll ignore
  for_ (resultDetails result) $ \details -> do
    let name = resultDetailsExperimentName details

    -- Store the timing for the control value,
    Statsd.timing ("science." <> name <> ".control")
      $ resultControlDuration
      $ resultDetailsControl details

    -- for the candidate (only the first, see "Breaking the rules" below,
    Statsd.timing ("science." <> name <> ".candidate")
      $ resultCandidateDuration
      $ resultDetailsCandidate details

  -- and counts for match/ignore/mismatch:
  case result of
    ResultMatched details -> do
      Statsd.increment $ "science." <> name <> ".matched"
    ResultIgnored details -> do
      Statsd.increment $ "science." <> name <> ".ignored"
    ResultMismatched details -> do
      Statsd.increment $ "science." <> name <> ".mismatched"
      -- Finally, store mismatches in redis so they can be retrieved and
      -- examined later on, for debugging and research.
      storeMismatchData details

storeMismatchData :: ResultDetails MyContext User User -> m ()
storeMismatchData details = do
  let
    name = resultDetailsExperimentName details
    context = resultDetailsExperimentContext details

    payload = MyPayload
      { name = name
      , context = context
      , control = controlObservationPayload(resultDetailsControl details)
      , candidate = candidateObservationPayload(resultDetailsCandidate details)
      , execution_order => result.observations.map(&:name)
      }

    key = "science.#{name}.mismatch"

  Redis.lpush key payload
  Redis.ltrim key 0 1000

controlObservationPayload :: ResultControl User -> Value
controlObservationPayload rc =
  object ["value" .= cleanValue $ resultControlValue user]

candidateObservationPayload :: ResultCandidate User -> Value
candidateObservationPayload = \case
  Left ex -> object ["exception" .= displayException ex]
  Right rc -> object ["value" .= cleanValue $ resultCandidateValue user]

-- See "Keeping it clean" above
cleanValue :: User -> Text
cleanValue = undefined
```

See `Result`, `ResultDetails`, `ResultControl` and `ResultCandidate` for all the
available data you can publish.

### [Testing](https://github.com/github/scientist#testing)

**TODO**: `raise_on_mismatches`

#### [Custom mismatch errors](https://github.com/github/scientist#custom-mismatch-errors)

**TODO**: `raise_with`

### [Handling errors](https://github.com/github/scientist#handling-errors)

#### [In candidate code](https://github.com/github/scientist#in-candidate-code)

Candidate code is wrapped in `tryAny`, resulting in `Either SomeException`
values in the result candidates list. We use the [safer][blog]
`UnliftIO.Exception` module.

[blog]: https://www.fpcomplete.com/haskell/tutorial/exceptions/

#### [In a Scientist callback](https://github.com/github/scientist#in-a-scientist-callback)

See `setExperimentOnException`.

## [Breaking the rules](https://github.com/github/scientist#breaking-the-rules)

### [Ignoring results entirely](https://github.com/github/scientist#ignoring-results-entirely)

```haskell
nope0 :: Experiment m c a b -> Experiment m c a b
nope0 = setExperimentIgnore (\_ _ -> True)
```

Or, more efficiently:

```haskell
nope1 :: Experiment m c a b -> Experiment m c a b
nope1 = setExperimentCompare (\_ _ -> True)
```

### [Trying more than one thing](https://github.com/github/scientist#trying-more-than-one-thing)

If you call `setExperimentTry` more than once, it will append (not overwrite)
candidate branches. If any candidate is deemed ignored or a mismatch, the
overall result will be.

**NOTE**: We do not support naming `try` branches.

### [No control, just candidates](https://github.com/github/scientist#no-control-just-candidates)

Not supported, since we don't support naming `try` branches.

<!--
```haskell
main :: IO ()
main = pure ()
```
-->

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)