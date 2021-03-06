// Brunch automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "brunch-config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".
import "phoenix_html"

import Elm from "./main.js";

// FB SDK Setup/Functions
const fbId = document.querySelector("[data-fb-id]").getAttribute('data-fb-id');

window.fbAsyncInit = () => {
  FB.init({
    appId      : `${fbId}`,
    xfbml      : true,
    version    : 'v2.9'
  });
  FB.AppEvents.logPageView();
};

(function(d, s, id){
   var js, fjs = d.getElementsByTagName(s)[0];
   if (d.getElementById(id)) {return;}
   js = d.createElement(s); js.id = id;
   js.src = "//connect.facebook.net/en_US/sdk.js";
   fjs.parentNode.insertBefore(js, fjs);
 }(document, 'script', 'facebook-jssdk'));

const ELM_DIV = document.getElementById("elm-div");
let elmApp = Elm.Main.embed(ELM_DIV, {
    session: localStorage.session,
    socketUrl: document.querySelector("[data-ws-url]").getAttribute('data-ws-url'),
    apiUrl: document.querySelector("[data-api-url]").getAttribute('data-api-url')
});

elmApp.ports.storeSession.subscribe((session) => {
  localStorage.session = session;
});

elmApp.ports.scrollChatToTop.subscribe(() => {
	let chat = document.getElementById('chat');
	chat ? chat.scrollTo(0, 0) : null;
});

elmApp.ports.logout.subscribe(() => {
	localStorage.removeItem('session');
});

elmApp.ports.triggerFBInviteRequest.subscribe(() => {
  if (window.FB) {
    FB.ui({method: 'apprequests',
      message: 'Join me for a game of Poker on PokerEX!'
    }, (response) => { "ok" });
  }
});

elmApp.ports.loginWithFB.subscribe(() => {
  FB.login((response) => {
    if (response.authResponse) {
      FB.api("/me", (response) => {
        elmApp.ports.onFBLogin.send(response);
      });
    }
  });
});

window.addEventListener("storage", (event) => {
  if (event.storageArea === localStorage && event.key === "session") {
    app.ports.onSessionChange.send(event.newValue);
  }
}, false);
