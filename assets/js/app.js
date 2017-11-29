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

const ELM_DIV = document.getElementById("elm-div");
let elmApp = Elm.Main.embed(ELM_DIV, localStorage.session || null);

elmApp.ports.storeSession.subscribe((session) => {
  console.log("Store session port was called with: ", JSON.stringify(session));
  localStorage.session = session;
});

elmApp.ports.scrollChatToTop.subscribe(() => {
	let chat = document.getElementById('chat');
	chat ? chat.scrollTo(0, 0) : null;
})

window.addEventListener("storage", (event) => {
  if (event.storageArea === localStorage && event.key === "session") {
  	console.log("onSessionChange received with event: ", event.newValue);
    app.ports.onSessionChange.send(event.newValue);
  }
}, false);
// Import local files
//
// Local files can be imported directly using relative
// paths "./socket" or full ones "web/static/js/socket".

// import socket from "./socket"
