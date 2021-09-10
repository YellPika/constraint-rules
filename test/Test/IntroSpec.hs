{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnicodeSyntax    #-}

module Test.IntroSpec where

import Control.Exception      (evaluate)
import Control.Exception.Base (TypeError)
import Test.Hspec             (Spec, describe, it, shouldBe, shouldThrow)
import Test.Util              (BadProof, exc)

import Test.IntroDefs

spec ∷ Spec
spec = do
  describe "test₁" do
    it "should not type check" do
      evaluate (test₁ @10) `shouldThrow` exc @TypeError

  describe "test₂" do
    it "solves KnownNat (10 + 5) with bad proof" do
      evaluate (test₂ @10) `shouldThrow` exc @BadProof

  describe "test₃" do
    it "solves KnownNat (10 + 5) with plusNat" do
      test₃ @10 `shouldBe` 15

  describe "nested₁" do
    it "should not type check" do
      evaluate (nested₁ @10 @7) `shouldThrow` exc @TypeError

  describe "nested₂" do
    it "solves KnownNat (10 + 7 + 5) with plusNat" do
      nested₂ @10 @7 `shouldBe` 22

  describe "multiple₁" do
    it "should not type check" do
      evaluate (multiple₁ @10 @7) `shouldThrow` exc @TypeError

  describe "multiple₂" do
    it "solves KnownNat (10 * 7 + 5) with plusNat and timesNat" do
      multiple₂ @10 @7 `shouldBe` 75
