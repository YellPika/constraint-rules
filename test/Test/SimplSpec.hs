{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnicodeSyntax    #-}

module Test.SimplSpec where

import Control.Exception      (evaluate)
import Control.Exception.Base (TypeError)
import Test.Hspec             (Spec, describe, it, shouldBe, shouldThrow)
import Test.Util              (BadProof, exc)

import Test.SimplDefs

spec ∷ Spec
spec = do
  describe "wanted₁" do
    it "should not type check" do
      evaluate wanted₁ `shouldThrow` exc @TypeError

  describe "wanted₂" do
    it "rewrites wanted equality with bad proof" do
      evaluate wanted₂ `shouldThrow` exc @BadProof

  describe "wanted₃" do
    it "rewrites wanted equality with trustMe" do
      wanted₃ `shouldBe` Nothing

  describe "wanted₄" do
    it "should not type check" do
      evaluate wanted₄ `shouldThrow` exc @TypeError

  describe "wanted₅" do
    it "rewrites wanted class with bad proof" do
      evaluate wanted₅ `shouldThrow` exc @BadProof

  describe "wanted₆" do
    it "rewrites wanted class with trustMe" do
      wanted₆ `shouldBe` ()

  describe "given₁" do
    it "should not type check" do
      evaluate given₁ `shouldThrow` exc @TypeError

  describe "given₂" do
    it "rewrites given class with bad proof" do
      evaluate given₂ `shouldThrow` exc @BadProof

  describe "given₃" do
    it "rewrites given class with trustMe" do
      given₃ `shouldBe` ()

  describe "nested₁" do
    it "should not type check" do
      evaluate nested₁ `shouldThrow` exc @TypeError

  describe "nested₂" do
    it "performs nested rewrites" do
      nested₂ `shouldBe` ()

  describe "multiple₁" do
    it "should not type check" do
      evaluate multiple₁ `shouldThrow` exc @TypeError

  describe "multiple₂" do
    it "rewrites multiple constraints" do
      multiple₂ `shouldBe` ()
