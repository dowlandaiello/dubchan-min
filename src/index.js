import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import GUN from 'gun';
import SEA from 'gun/sea';

const gun = GUN({peers: ['https://dubchan.herokuapp.com/gun']});

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.loadPost.subscribe((m) => {
  gun.get('#').get(m).once((postStr) => {
    if (postStr !== undefined) {
      try {
        const post = { ...JSON.parse(postStr), id: m, nComments: 0 };
        const epoched = { ...post, nonce: post.nonce ?? 0, hash: post.hash ?? "" };

        if (epoched.content === null) {
          app.ports.postLoaded.send({ ...epoched, comments: [] });
        } else {
          app.ports.postLoaded.send({ ...epoched, comments: [], content: post.content });
        }
      } catch (e) {
        console.error(e);
      }
    }
  });
});

gun.get('#').map().once((str, id) => {
  try {
    const json = JSON.parse(str);
    if (json.parent !== undefined) {
      app.ports.commentIn.send({ ...json, id: id, nonce: json.nonce ?? 0, hash: json.hash ?? "", content: json.content ?? null });
    }

    // This is a post
    if (json.content !== undefined) {
      const post = json;
      const sanitized = { timestamp: post.timestamp, title: post.title, text: post.text, id: id, comments: null, content: null, nComments: 0, nonce: post.nonce ?? 0, hash: post.hash ?? "" };

      if (post.content !== null) {
        const rich = { ...sanitized, content: post.content };

        app.ports.postIn.send(rich);
      } else {
        app.ports.postIn.send(sanitized);
      }
    }
  } catch (e) {
    console.error(e);
  }
});

app.ports.submitPost.subscribe(async (post) => {
  const data = JSON.stringify(post);
  const hash = await SEA.work(data, null, null, { name: "SHA-256" });

  const inserted = gun.get('#').get(hash).put(data);
  gun.get('posts').set(hash);
});

app.ports.submitComment.subscribe(async (comment) => {
  const data = JSON.stringify(comment);
  const hash = await SEA.work(data, null, null, { name: "SHA-256" });

  const inserted = gun.get('#').get(hash).put(data)
  gun.get('comments').get(comment.parent).set(hash);
});

window.gun = gun;
window.sea = SEA;

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
