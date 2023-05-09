import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import GUN from 'gun';
import SEA from 'gun/sea';
import captcha from "trek-captcha";

const gun = GUN({peers: ['https://dubchan.herokuapp.com/gun']});

const scrollDebounce = 100;
let debounceTimer = null;

const app = Elm.Main.init({
  node: document.getElementById('root')
});

const chunk = (timestamp) => Math.floor(timestamp / 86400) * 86400;

app.ports.copy.subscribe((s) => {
  navigator.clipboard.writeText(s);
});

app.ports.loadPost.subscribe((m) => {
  gun.get('#posts').get(m).once((postStr) => {
    if (postStr !== undefined) {
      try {
        const post = { ...JSON.parse(postStr), id: m, nComments: 0, uniqueFactor: 0.0 };
        const epoched = { ...post, nonce: post.nonce ?? 0, hash: post.hash ?? "", prev: post.prev, captcha: post.captcha };

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

app.ports.getComments.subscribe((post) => {
  gun.get('#comments/' + post).map().once((str, id) => {
    try {
      const json = JSON.parse(str);
      if (json.parent !== undefined) {
        app.ports.commentIn.send({ ...json, id: id, nonce: json.nonce ?? 0, hash: json.hash ?? "", content: json.content ?? null });
      }
    } catch (e) {
      console.error(e);
    }
  });
});

const loadChunk = (timestamp) => {
  gun.get('#posts').get({ '.': { '*': timestamp.toString() }}).map().once(async (str, id) => {
    try {
      const json = JSON.parse(str);
      const id = await SEA.work(str, null, null, { name: 'SHA-256' });

      // This is a post
      if (json.title !== undefined) {
        const post = json;
        const sanitized = { timestamp: post.timestamp, title: post.title, text: post.text, id: id, comments: null, content: null, nComments: 0, nonce: post.nonce ?? 0, hash: post.hash ?? "", uniqueFactor: 0.0, prev: post.prev, captcha: post.captcha };

        if (post.content !== null) {
          const rich = { ...sanitized, content: post.content };

          app.ports.postIn.send({ timestamp: timestamp, post: rich });
        } else {
          app.ports.postIn.send({ timestamp: timestamp, post: sanitized });
        }
      }
    } catch (e) {
      console.error(e);
    }
  });
};

const genCaptcha = async () => {
  const { token, buffer } = await captcha();
  console.log(token, buffer);
  //const parsed = { answer: token, data: /captcha.data };
  //app.ports.gotCaptcha.send(parsed);
};

app.ports.loadChunk.subscribe(loadChunk);
app.ports.genCaptcha.subscribe(genCaptcha);

app.ports.submitPost.subscribe(async (post) => {
  const data = JSON.stringify(post);
  const hash = await SEA.work(data, null, null, { name: 'SHA-256' });

  gun.get('#posts').get(chunk(post.timestamp) + '#' + hash).put(data);
  gun.get('#posts').get(hash).put(data);
});

app.ports.submitComment.subscribe(async ([comment, rawParent]) => {
  const data = JSON.stringify(comment);
  const parent = { ...rawParent, id: rawParent.hash };
  const parentData = JSON.stringify(parent);
  const hash = await SEA.work(data, null, null, { name: "SHA-256" });
  const parentHash = await SEA.work(parentData, null, null, { name: "SHA-256" });

  gun.get('#comments/' + comment.parent).get(hash).put(data);
  gun.get('#posts').get(chunk(comment.timestamp) + '#' + parentHash).put(parentData);
});

const feed = document.getElementsByClassName("feedContainer")[0];

setTimeout(() => {
  feed.addEventListener('scroll', (e) => {
    clearTimeout(debounceTimer);
    debounceTimer = setTimeout(() => {
      if (feed.scrollTop + feed.clientHeight >= feed.scrollHeight * 0.8) {
        app.ports.scrolledBottom.send(true);
      }
    }, scrollDebounce);
  }, { passive: true });
}, 300);

setTimeout(() => {
  if (feed.scrollTop + feed.clientHeight >= feed.scrollHeight * 0.8) {
        app.ports.scrolledBottom.send(true);
  }
}, 1000);

genCaptcha();

window.gun = gun;
window.sea = SEA;

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
