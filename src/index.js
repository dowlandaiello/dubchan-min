import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import GUN from 'gun';
import SEA from 'gun/sea';
import 'gun/lib/radix';
import 'gun/lib/radisk';
import 'gun/lib/store';
import 'gun/lib/rindexed';
import { createLibp2p } from 'libp2p';
import { circuitRelayTransport } from 'libp2p/circuit-relay';
import { identifyService } from 'libp2p/identify';
import { kadDHT } from '@libp2p/kad-dht';
import { webSockets } from '@libp2p/websockets';
import { webTransport } from '@libp2p/webtransport';
import { webRTCDirect, webRTC } from '@libp2p/webrtc';
import { noise } from '@chainsafe/libp2p-noise';
import { mplex } from '@libp2p/mplex';
import { yamux } from '@chainsafe/libp2p-yamux';
import { bootstrap } from '@libp2p/bootstrap';
import { loadCaptcha, genCaptcha } from './captcha';

const gun = GUN({peers: ['https://dubchan.herokuapp.com/gun'], localStorage: false});

const scrollDebounce = 100;
let debounceTimer = null;

const app = Elm.Main.init({
  node: document.getElementById('root')
});

const main = async () => {
  const chunk = (timestamp) => Math.floor(timestamp / 86400) * 86400;

  app.ports.copy.subscribe((s) => {
    navigator.clipboard.writeText(s);
  });

  app.ports.loadCaptcha.subscribe(loadCaptcha(gun, app));

  app.ports.loadPost.subscribe((m) => {
    gun.get('#posts').get(m).once((postStr) => {
      if (postStr !== undefined) {
        try {
          const post = { ...JSON.parse(postStr), id: m, nComments: 0, uniqueFactor: 0.0 };
          const epoched = { ...post, nonce: post.nonce ?? 0, hash: post.hash ?? "", prev: post.prev, captcha: post.captcha };

          console.log(epoched);

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

  app.ports.getComments.subscribe(async (post) => {
    const loadComments = (str, id) => {
      try {
        const json = JSON.parse(str);
        if (json.parent !== undefined) {
          gun.get('#comments').get(id).put(str);

          app.ports.commentIn.send({ ...json, id: id, nonce: json.nonce ?? 0, hash: json.hash ?? "", content: json.content ?? null, captchaAnswer: json.captchaAnswer });
        }
      } catch (e) {
        console.error(e);
      }
    };

    gun.get('#comments/' + post).map().once(loadComments);
  });

  const loadChunk = (timestamp) => {
    console.log(timestamp);
    gun.get('#chunk_' + timestamp.toString()).map().once(async (str, id) => {
      try {
        const json = JSON.parse(str);
        const id = await SEA.work(str, null, null, { name: 'SHA-256' });

        // This is a post
        if (json.title !== undefined) {
          const post = json;
          const sanitized = { timestamp: post.timestamp, title: post.title, text: post.text, id: id, comments: null, content: null, nComments: 0, nonce: post.nonce ?? 0, hash: post.hash ?? "", uniqueFactor: 0.0, prev: post.prev, captcha: post.captcha, captchaAnswer: post.captchaAnswer };

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

  app.ports.loadChunk.subscribe(loadChunk);
  app.ports.genCaptcha.subscribe(genCaptcha(app));

  app.ports.submitPost.subscribe(async (post) => {
    const data = JSON.stringify(post);
    const hash = await SEA.work(data, null, null, { name: 'SHA-256' });

    gun.get('#chunk_' + chunk(post.timestamp)).get(hash).put(data);
    gun.get('#posts').get(hash).put(data);
  });

  app.ports.submitComment.subscribe(async ([comment, rawParent]) => {
    gun.get('#posts').get(rawParent.id).once(async (parentStr, id) => {
      const data = JSON.stringify(comment);
      const hash = await SEA.work(data, null, null, { name: 'SHA-256' });

      gun.get('#comments/' + comment.parent).get(hash).put(data);
      gun.get('#chunk_' + chunk(comment.timestamp)).get(id).put(parentStr);
      gun.get('#comments').get(hash).put(data);
    });
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
};

main();

window.gun = gun;
window.sea = SEA;

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
