{-# LANGUAGE CPP #-}

module Data.Time.Format.HumanSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Data.Time.Format.Human

import Data.Maybe (fromJust)
import Data.Time
import Data.Time.Format (parseTime)

#if !MIN_VERSION_time(1,5,0)
import System.Locale (defaultTimeLocale)
parseTimeM _ = parseTime
#endif


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "humanReadableTime'" $ do
    it "returns just now for near times" $ do
        let n = parseTime' "2015-01-01 01:00:00.1"
            t = parseTime' "2015-01-01 01:00:00"

        humanReadableTime' n t `shouldBe` "just now"
        humanReadableTime' t n `shouldBe` "just now"

        humanReadableTimeI18N' finnishHumanTimeLocale n t `shouldBe` "juuri nyt"

    it "returns seconds for times less than a minute" $ do
        let n = parseTime' "2015-01-01 01:00:59"
        let t = parseTime' "2015-01-01 01:00:00"

        humanReadableTime' n t `shouldBe` "59 seconds ago"
        humanReadableTime' t n `shouldBe` "59 seconds from now"

        humanReadableTimeI18N' finnishHumanTimeLocale n t `shouldBe` "59 sekuntia sitten"
        humanReadableTimeI18N' finnishHumanTimeLocale t n `shouldBe` "59 sekunnin kuluttua"

    it "returns minutes for times less than an hour" $ do
        let n = parseTime' "2015-01-01 01:59:00"
        let t = parseTime' "2015-01-01 01:00:00"

        humanReadableTime' n t `shouldBe` "59 minutes ago"
        humanReadableTime' t n `shouldBe` "59 minutes from now"

        humanReadableTimeI18N' finnishHumanTimeLocale n t `shouldBe` "59 minuuttia sitten"
        humanReadableTimeI18N' finnishHumanTimeLocale t n `shouldBe` "59 minuutin kuluttua"

    it "returns hours for times less than a day" $ do
        let n = parseTime' "2015-01-01 23:59:00"
        let t = parseTime' "2015-01-01 01:00:00"

        humanReadableTime' n t `shouldBe` "about 22 hours ago"
        humanReadableTime' t n `shouldBe` "about 22 hours from now"

        humanReadableTimeI18N' finnishHumanTimeLocale n t `shouldBe` "noin 22 tuntia sitten"
        humanReadableTimeI18N' finnishHumanTimeLocale t n `shouldBe` "noin 22 tunnin kuluttua"

    context "when less than 5 days " $ do
        let n = parseTime' "2015-01-04 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        it "returns day of week in EDT" $ do
            let l = defaultHumanTimeLocale { timeZone = read "EDT" }

            humanReadableTimeI18N' l n t `shouldBe` "at 9:00 PM on Wednesday"
            humanReadableTimeI18N' l t n `shouldBe` "at 9:00 PM on Saturday"

        it "returns day of week in India" $ do
            let l = defaultHumanTimeLocale { timeZone = read "+0530" }

            humanReadableTimeI18N' l n t `shouldBe` "at 6:30 AM on Thursday"
            humanReadableTimeI18N' l t n `shouldBe` "at 6:30 AM on Sunday"

        it "returns day of week in Finland for Finnish time locale" $ do
            humanReadableTimeI18N' finnishHumanTimeLocale n t `shouldBe` "torstaina klo  3.00" -- TODO: spacing
            humanReadableTimeI18N' finnishHumanTimeLocale t n `shouldBe` "sunnuntaina klo  3.00"

    it "returns days for times less than 10 days" $ do
        let n = parseTime' "2015-01-10 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        humanReadableTime' n t `shouldBe` "9 days ago"
        humanReadableTime' t n `shouldBe` "9 days from now"

        humanReadableTimeI18N' finnishHumanTimeLocale n t `shouldBe` "9 päivää sitten"
        humanReadableTimeI18N' finnishHumanTimeLocale t n `shouldBe` "9 päivän kuluttua"

    it "returns weeks for times less than 5 weeks" $ do
        let n = parseTime' "2015-01-29 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        humanReadableTime' n t `shouldBe` "4 weeks ago"
        humanReadableTime' t n `shouldBe` "4 weeks from now"

        humanReadableTimeI18N' finnishHumanTimeLocale n t `shouldBe` "4 viikkoa sitten"
        humanReadableTimeI18N' finnishHumanTimeLocale t n `shouldBe` "4 viikon kuluttua"

    it "returns a date string without year for this year" $ do
        let n = parseTime' "2015-12-30 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        humanReadableTime' n t `shouldBe` "on Jan  1" -- TODO: spacing
        humanReadableTime' t n `shouldBe` "on Dec 30"

        humanReadableTimeI18N' finnishHumanTimeLocale n t `shouldBe` "1. tammikuuta"
        humanReadableTimeI18N' finnishHumanTimeLocale t n `shouldBe` "30. joulukuuta"

    it "returns a date string including the year for previous years" $ do
        let n = parseTime' "2025-12-30 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        humanReadableTime' n t `shouldBe` "on Jan  1, 2015" -- TODO: spacing
        humanReadableTime' t n `shouldBe` "on Dec 30, 2025"

        humanReadableTimeI18N' finnishHumanTimeLocale n t `shouldBe` "1. tammikuuta, 2015"
        humanReadableTimeI18N' finnishHumanTimeLocale t n `shouldBe` "30. joulukuuta, 2025"

parseTime' :: String -> UTCTime
parseTime' = fromJust . parseTimeM True defaultTimeLocale "%F %T%Q"
