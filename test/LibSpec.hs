module LibSpec where

-- import Data.Hourglass
import Data.Time
import Data.Fixed
import Lib
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "parseTimeShift" $ do
    let itParses s e = it ("parses `" ++ s ++ "`") $ parseTimeShift s `shouldBe` (Right e)
    itParses "+1h" (Offset 3600)
    itParses "+5m" (Offset 300)
    itParses "+42s" (Offset 42)
    itParses "-1h" (Offset (-3600))
    itParses "-5m" (Offset (-300))
    itParses "-42s" (Offset (-42))
    itParses "#42" FortyTwo
  -- itParses "00:00..12:00" (TimeRange midnight midday)
  describe "applyTimeShift" $ do
    it "does not change the time zone" $ do
      property $ \t timeShift ->
        zonedTimeZone (applyTimeShift timeShift t) `shouldBe` zonedTimeZone t
    context "when given an offset" $ do
      it "shifts time by the offset value" $
        property $ \t -> forAll genOffset $ \offset ->
          let result = applyTimeShift offset t
           in diffTime result t `shouldBe` fromInteger (offsetSeconds offset)
    context "when given 42" $ do
      it "handles a simple example" $ do
        let time = read "2020-01-02 03:04:05 +0200" :: ZonedTime
        show (applyTimeShift FortyTwo time) `shouldBe` "2020-01-02 03:04:42 +0200"
      it "changes the number of seconds to 42" $ do
        property $ \t ->
          let result = applyTimeShift FortyTwo t
           in todSec (localTimeOfDay (zonedTimeToLocalTime result)) `shouldBe` 42
      it "stays in the same minute" $ do
        property $ \time -> do
          let result = applyTimeShift FortyTwo time
          show (setSeconds 0 result) `shouldBe` show (setSeconds 0 time)

--   context "when given an time range" $ do
--     it "shifts a single datetime to the first time" $ do
--       let times = map read [ "2001-01-01 00:00:00 +0100" ]
--           timeShift = TimeRange (read "12:34:56") (read "22:22:22")
--           expected = map read [ "12:34:56" ]
--       map (localTimeOfDay . zonedTimeToLocalTime) (applyTimeShift timeShift times) `shouldBe` expected
--     it "shifts times between start and end preserving ratios" $ do
--       let times = map read [ "2001-01-01 00:00:00 +0100"
--                            , "2001-01-01 01:00:00 +0100"
--                            , "2001-01-01 04:00:00 +0100"
--                            , "2001-01-01 10:00:00 +0100"
--                            ]
--           timeShift = TimeRange (read "12:00:00") (read "13:00:00")
--           expected = map read [ "12:00:00" , "12:06:00" , "12:24:00" , "13:00:00" ]
--       map (localTimeOfDay . zonedTimeToLocalTime) (applyTimeShift timeShift times) `shouldBe` expected
--     it "shifts all times after the start of the range" $ do
--       property $ \times -> length times > 0 ==> forAll genTimeRange $ \timeRange -> do
--         let result = applyTimeShift timeRange times
--         minimum ( map roundedTimeOfDay result) `shouldBe` timeRangeStart timeRange
--     it "shifts all times before the end of the range" $ do
--       property $ \times -> length times > 1 ==> forAll genTimeRange $ \timeRange -> do
--         let result = applyTimeShift timeRange times
--         maximum ( map roundedTimeOfDay result) `shouldBe` timeRangeEnd timeRange
--     it "leaves the day unchanged" $ do
--       property $ \times -> forAll genTimeRange $ \timeRange -> do
--         let result = applyTimeShift timeRange times
--         map zonedDay result `shouldBe` map zonedDay times

-- roundedTimeOfDay :: ZonedTime -> TimeOfDay
-- roundedTimeOfDay zt = let TimeOfDay h m s = zonedTimeOfDay zt
--                         seconds = round s
--                     in if seconds == 60
--                       then TimeOfDay h (m+1) 0
--                       else TimeOfDay h m (MkFixed $ round s * 10^12)

-- instance Arbitrary Seconds where
--   arbitrary = Seconds <$> choose (-2 ^ 55, 2 ^ 55 -1) -- See https://hackage.haskell.org/package/hourglass-0.2.12/docs/Time-Types.html#t:Seconds

-- instance Arbitrary TimezoneOffset where
--   arbitrary = TimezoneOffset <$> arbitrary

-- instance Arbitrary Elapsed where
--   arbitrary = Elapsed <$> arbitrary

-- instance Arbitrary DateTime where
--   arbitrary = timeConvert <$> (arbitrary :: Gen Elapsed)

-- instance (Time t, Arbitrary t) => Arbitrary (LocalTime t) where
--   arbitrary = localTime <$> arbitrary <*> arbitrary

shouldAllBe :: (HasCallStack, Show a, Eq a) => [a] -> a -> Expectation
shouldAllBe actuals expected = mapM_ (flip shouldBe expected) actuals

diffTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffTime t1 t2 = diffUTCTime (zonedTimeToUTC t1) (zonedTimeToUTC t2)

instance Arbitrary ZonedTime where
  arbitrary = ZonedTime <$> arbitrary <*> arbitrary

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary TimeOfDay where
  arbitrary = do
    hours <- choose (0, 23)
    minutes <- choose (0, 59)
    seconds <- choose (0, 59)
    return $ TimeOfDay hours minutes (MkFixed $ seconds * 10^12)

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary TimeZone where
  arbitrary = minutesToTimeZone <$> arbitrary

-- -- ^ Generate a valid TimRange where the start time is before the end time
-- genTimeRange :: Gen TimeShift
-- genTimeRange = uncurry TimeRange <$> arbitrary `suchThat` (uncurry (<=))

-- Generate an Offset
genOffset :: Gen TimeShift
genOffset = Offset <$> arbitrary

instance Arbitrary TimeShift where
  arbitrary = oneof [pure FortyTwo ] -- genOffset] -- , genTimeRange]

-- instance Arbitrary NominalDiffTime where
-- arbitrary = fromInteger <$> arbitrary
