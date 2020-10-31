module Main where

import Data.Semigroup ((<>))
import Git hiding (Commit)
import Git.Libgit2
import Lib
import Options.Applicative

data Opts = Opts
  {optsTimeShift :: TimeShift}
  deriving (Show)

opts :: Parser Opts
opts = Opts <$> argument (eitherReader parseTimeShift) (metavar "TIMESHIFT")

main :: IO ()
main = do
  options <- execParser $ info (opts <**> helper) (fullDesc <> progDesc "TODO")
  print options
  repo <- openLgRepository defaultRepositoryOptions {repoPath = "."}
  print $ lgRepoPath repo
  runLgRepository repo $ do
    (ref, commit) <- peelToCommit "HEAD"
    lgDebug $ show $ updateSignature (optsTimeShift options) $ commitAuthor commit
    commit' <-
      createCommit
        (commitParents commit)
        (commitTree commit)
        (updateSignature (optsTimeShift options) $ commitAuthor commit)
        (updateSignature (optsTimeShift options) $ commitCommitter commit)
        (commitLog commit)
        (Just ref)

    lgDebug $ show $ commitOid commit'
    lgDebug "Foobar"
    pure ()

updateSignature :: TimeShift -> Signature -> Signature
updateSignature ts s = s {signatureWhen = applyTimeShift ts (signatureWhen s)}

peelToCommit :: (HasLgRepo m, MonadLg m) => RefName -> m (RefName, Commit)
peelToCommit name = do
  r <- lookupReference name
  case r of
    Nothing -> error "No such reference"
    Just (RefSymbolic name') -> peelToCommit name'
    Just (RefObj oid) -> do
      obj <- lookupObject oid
      case obj of
        CommitObj commit -> pure (name, commit)
        _ -> error "not a commit"
