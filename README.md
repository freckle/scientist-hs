# Scientist

Haskell port of
[`github/scientist`](https://github.com/github/scientist#readme).

## How do I science?

```hs
myWidgetAllows :: Model -> User -> m Bool
myWidgetAllows model user = do
  experimentRun
    $ setExperimentTry
        (userCanRead user model) -- new way
    $ newExperiment "widget-permissions"
        (modelCheckUserValid model user) -- old way
```

## Making science useful

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

### Controlling comparison

```hs
experimentRun
  $ setExperimentCompare (experimentCompareBy userLogin)
  $ setExperimentTry userServiceFetch
  $ newExperiment "users" fetchAllUsers
```

If a candidate branch raises an exception, that will never compare equally.

(Exceptions in the control branch are never caught by us, so there's no need to
consider them here.)

### Adding context

See `setExperimentContext`.

### Expensive setup

Just do it ahead of time.

```hs
x <- expensiveSetup

experimentRun
  $ setExperimentTry (newCode x)
  $ newExperiment "expensive" (originalCode x)
```

### Keeping it clean

Not supported at this time. Format the value(s) as necessary when publishing.

### Ignoring mismatches

See `setExperimentIgnore`.

### Enabling/disabling experiments

See `setExperimentRunIf`.

### Ramping up experiments

```hs
experimentRun
  $ setExperimentEnabled (experimentEnabledPercent 30)
  -- ...
```

### Publishing results

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

### Testing

TODO: `raise_on_mismatches` & `raise_with`

### Handling errors

#### In candidate code

Candidate code is wrapped in `tryAny`, resulting in `Either SomeException`
values in the result candidates list.

#### In a Scientist callback

See `setExperimentOnException`.

## Breaking the rules

### Ignoring results entirely

```hs
setExperimentIgnore (\_ _ -> True)
```

Or, more efficiently:

```hs
setExperimentCompare (\_ _ -> True)
```

### Trying more than one thing

If you call `setExperimentTry` more than once, it will append (not overwrite)
candidate branches. If any candidate is deemed ignored or a mismatch, the
overall result will be.

NOTE: We do not support naming `try` branches.

### No control, just candidates

Not supported, since we don't support naming `try` branches.

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
