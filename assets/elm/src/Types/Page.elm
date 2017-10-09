module Types.Page exposing (..)

import Page.Login as Login
import Page.Register as Register
import Page.Home as Home

type Page
  = Blank
  | NotFound
  | Login Login.Model
  | Register Register.Model
  | Home Home.Model