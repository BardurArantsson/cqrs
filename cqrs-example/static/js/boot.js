import React from "react";
import Application from "./components/Application";

// FIXME: superagent doesn't work for some reason if installed via npm + included via "import"...

(function() {

  "use strict";

  React.render(
    <Application />,
    document.body);

}());
