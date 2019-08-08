import { Elm } from './Main.elm';
import './main.scss';
import touchInputManager from './touchInputManager';

//  localStorage
const storageKey = "elm2048";

const app = Elm.Main.init({
  node: document.getElementById('elm-node')
});


touchInputManager(app);
