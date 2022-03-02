# Scientist

Haskell port of
[`github/scientist`](https://github.com/github/scientist#readme).

## Usage

1. Define a new `Experiment m c a b` with,

   ```hs
   newExperiment "some name" theOriginalCode
   ```

   The type variables capture the following details:

   - `m`: Some Monad to operate in, e.g. `IO`.
   - `c`: Any context value you attach with `setExperimentContext`. It will then
     be available in the `Result c a b` you publish.
   - `a`: The result type of your original (aka "control") code, this is what is
     always returned and so is the return type of `experimentRun`.
   - `b`: The result type of your experimental (aka "candidate") code. It
     probably won't differ (and must not to use a comparison like `(==)`), but
     it can (provided you implement a comparison between `a` and `b`).

1. Configure the Experiment as desired

   ```hs
   setExperimentPublish publish
     $ setExperimentCompare experimentCompareBy
     $ setExperimentTry theExperimentalCode
     $ newExperiment "some name" theOriginalCode

   -- Increment Statsd, Log, store in Redis, whatever
   publish :: Result c a b -> m ()
   publish = undefined
   ```

1. Run the experiment

   ```hs
   -- Returns the same `m a` as `theOriginalCode`
   experimentRun
     $ setExperimentPublish publish
     $ setExperimentCompare experimentCompareBy
     $ setExperimentTry theExperimentalCode
     $ newExperiment "some name" theOriginalCode
   ```

1. Explore things like `setExperimentIgnore`, `setExperimentEnabled`, etc.

---

The rest of this README matches section-by-section to the ported project and
shows only the differences in syntax for those features. Please follow the
header links for additional details, motivation, etc.

## [How do I science?](https://github.com/github/scientist#how-do-i-science)

```hs
myWidgetAllows :: Model -> User -> m Bool
myWidgetAllows model user = do
  experimentRun
    $ setExperimentTry
        (userCanRead user model) -- new way
    $ newExperiment "widget-permissions"
        (modelCheckUserValid model user) -- old way
```

## [Making science useful](https://github.com/github/scientist#making-science-useful)

```hs
experimentRun
  $ setExperimentEnabled (pure True)
  $ setExperimentOnException onScientistException
  $ setExperimentPublish (putStrLn . formatResult)
  -- ...

onScientistException :: ResultControl a -> SomeException -> IO (Result c a b)
onScientistException r ex = do
  putStringLn "..."

  -- To re-raise
  throwIO ex

  -- To ignore
  pure $ ResultSkipped $ Control $ resultControlValue r

formatResult :: Result c a b -> String
formatResult = undefined
```

### [Controlling comparison](https://github.com/github/scientist#controlling-comparison)

```hs
experimentRun
  $ setExperimentCompare (experimentCompareOn userLogin)
  $ setExperimentTry userServiceFetch
  $ newExperiment "users" fetchAllUsers
```

If a candidate branch raises an exception, that will never compare equally.

(Exceptions in the control branch are never caught by us, so there's no need to
consider them here.)

### [Adding context](https://github.com/github/scientist#adding-context)

See `setExperimentContext`.

### [Expensive setup](https://github.com/github/scientist#expensive-setup)

Just do it ahead of time.

```hs
x <- expensiveSetup

experimentRun
  $ setExperimentTry (newCode x)
  $ newExperiment "expensive" (originalCode x)
```

### [Keeping it clean](https://github.com/github/scientist#keeping-it-clean)

Not supported at this time. Format the value(s) as necessary when publishing.

### [Ignoring mismatches](https://github.com/github/scientist#ignoring-mismatches)

See `setExperimentIgnore`.

### [Enabling/disabling experiments](https://github.com/github/scientist#enablingdisabling-experiments)

See `setExperimentRunIf`.

### [Ramping up experiments](https://github.com/github/scientist#ramping-up-experiments)

```hs
experimentRun
  $ setExperimentEnabled (experimentEnabledPercent 30)
  -- ...
```

### [Publishing results](https://github.com/github/scientist#publishing-results)

```hs
experimentRun
  $ setExperimentPublish publish
  -- ...

publish :: MonadIO m => Results MyContext User User -> m ()
publish result = do
  case result of
    ResultSkipped{} -> pure ()

    ResultMatched details -> do
      let name = resultDetailsExperimentName details

      Statsd.timing ("science." <> name <> ".control")
        $ resultControlDuration
        $ resultDetailsControl details

      Statsd.timing ("science." <> name <> ".candidate")
        $ resultCandidateDuration
        $ NE.head -- See "Breaking the rules" below
        $ resultDetailsCandidates details

      Statsd.increment $ "science." <> name <> ".matched"

    ResultIgnored details -> do
      -- Same timing...
      Statsd.increment $ "science." <> name <> ".ignored"

    ResultMismatched details -> do
      -- Same timing...
      Statsd.increment $ "science." <> name <> ".mismatched"
      storeMismatchData result
```

### [Testing](https://github.com/github/scientist#testing)

**TODO**: `raise_on_mismatches`

#### [Custom mismatch errors](https://github.com/github/scientist#custom-mismatch-errors)

**TODO**: `raise_with`

### [Handling errors](https://github.com/github/scientist#handling-errors)

#### [In candidate code](https://github.com/github/scientist#in-candidate-code)

Candidate code is wrapped in `tryAny`, resulting in `Either SomeException`
values in the result candidates list.

#### [In a Scientist callback](https://github.com/github/scientist#in-a-scientist-callback)

See `setExperimentOnException`.

## [Breaking the rules](https://github.com/github/scientist#breaking-the-rules)

### [Ignoring results entirely](https://github.com/github/scientist#ignoring-results-entirely)

```hs
setExperimentIgnore (\_ _ -> True)
```

Or, more efficiently:

```hs
setExperimentCompare (\_ _ -> True)
```

### [Trying more than one thing](https://github.com/github/scientist#trying-more-than-one-thing)

If you call `setExperimentTry` more than once, it will append (not overwrite)
candidate branches. If any candidate is deemed ignored or a mismatch, the
overall result will be.

**NOTE**: We do not support naming `try` branches.

### [No control, just candidates](https://github.com/github/scientist#no-control-just-candidates)

Not supported, since we don't support naming `try` branches.

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
