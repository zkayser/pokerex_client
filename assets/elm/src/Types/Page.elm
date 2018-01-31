module Types.Page exposing (..)

import Page.ForgotPassword as ForgotPassword
import Page.Home as Home
import Page.Login as Login
import Page.Profile as Profile
import Page.Register as Register
import Page.ResetPassword as ResetPassword
import Page.Room as Room
import Page.Rooms as Rooms


type Page
    = Blank
    | NotFound
    | Login Login.Model
    | Register Register.Model
    | Home Home.Model
    | Rooms Rooms.Model
    | Room Room.Model
    | Profile Profile.Model
    | ForgotPassword ForgotPassword.Model
    | ResetPassword ResetPassword.Model
