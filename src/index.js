import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var settings = localStorage.getItem('settings');

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    settings: JSON.parse(settings),
    apiUrl: "http://api.kobonaut.com/feedback" // process.env.ELM_APP_API_URL
  }
});

app.ports.storeSettings.subscribe(function(data) {
  localStorage.setItem('settings', JSON.stringify(data));
});

registerServiceWorker();
