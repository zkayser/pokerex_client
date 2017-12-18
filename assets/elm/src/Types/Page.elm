module Types.Page exposing (..)

import Page.Login as Login
import Page.Register as Register
import Page.Home as Home
import Page.Room as Room
import Page.Rooms as Rooms
import Page.Profile as Profile

type Page
  = Blank
  | NotFound
  | Login Login.Model
  | Register Register.Model
  | Home Home.Model
  | Rooms Rooms.Model
  | Room Room.Model
  | Profile Profile.Model
