import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import GUN from 'gun';

const gun = GUN();

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.loadPost.subscribe((m) => {
  const post = gun.get('posts').get(m).once((post) => {
    if (post !== undefined) {
      app.ports.postLoaded.send(post);
    }
  });
});

gun.get('posts').map().once((post, _) => {
  if (post !== undefined) {
    const sanitized = { timestamp: post.timestamp, title: post.title, text: post.text, id: post.id, comments: null, content: null };

    if (post.content !== null) {
      gun.get(post.content).once((content) => {
        const rich = { ...sanitized, content: content };

        app.ports.postIn.send(rich);
      });

      return;
    }

    app.ports.postIn.send(sanitized);
  }
});

app.ports.submitPost.subscribe((post) => {
  const inserted = gun.get(post.id).put(post);
  gun.get('posts').set(inserted);
});

window.gun = gun;

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
