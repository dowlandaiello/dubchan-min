import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import GUN from 'gun';
import SEA from 'gun/sea';
import 'gun/lib/radix';
import 'gun/lib/radisk';
import 'gun/lib/store';
import 'gun/lib/rindexed';

const gun = GUN({peers: ['https://dubchan.herokuapp.com/gun', 'https://fathomless-chamber-82730.herokuapp.com/gun'], localStorage: false});

const scrollDebounce = 100;
let debounceTimer = null;

const app = Elm.Main.init({
  node: document.getElementById('root')
});

const chunk = (timestamp) => Math.floor(timestamp / 86400) * 86400;

app.ports.copy.subscribe((s) => {
  navigator.clipboard.writeText(s);
});

app.ports.loadCaptcha.subscribe((p) => {
  const n = parseInt(p.hash.substring(p.hash.length - 2, p.hash.length), 16);
  captchaFor(p, n, [], (chain) => {
    if (chain.length == 0)
      return;

    app.ports.loadedCaptcha.send({ post: p.hash, captcha: chain[n % chain.length].captcha });
  });
});

const base64 = b => btoa(String.fromCharCode.apply(null, new Uint8Array(b)));
const utfBytes = s => (new TextEncoder()).encode(s);

const captchaFor = (curr, n, chain, callback) => {
  if (n == 0) {
    callback(chain);
  }

  const analysis = (prevStr) => {
    if (prevStr === undefined) {
      callback(chain);

      return;
    }

    try {
      const prev = JSON.parse(prevStr);

      if (prev.captcha === undefined || prev.captcha.answer === undefined || prev.captcha.answer.length === 5 || prev.timestamp <= 1683831536) {
        callback(chain);

        return;
      }

      chain.push(prev);

      captchaFor(prev, n - 1, chain, callback);
    } catch (e) {
      console.error(e);
    }
  };

  if (curr.prev === undefined && curr.parent !== undefined) {
    rootFor(curr, (rootStr) => {
      try {
        const root = JSON.parse(rootStr);

        if (root.timestamp <= 1683831536) {
          gun.get('#posts').get('WpsHb7YL5nA8qeYRNCuJ6NmGIlCbMCkiZj0gowjJmXc=').once((alternateStr) => {
            try {
              const alternate = JSON.parse(alternateStr);
              captchaFor(alternate, n, chain, callback);
            } catch (e) {
              console.error(e);
            }
          });


          return;
        }

        analysis(rootStr);
      } catch (e) {
        console.error(e);
      }
    });
  }

  gun.get('#posts').get(curr.prev).once(analysis);
};

const rootFor = (curr, callback) => {
  gun.get('#comments').get(curr.parent).once((prevStr) => {
    try {
      const prev = JSON.parse(prevStr);

      rootFor(prev, callback);
    } catch (e) {
      console.error(e);
    }
  });

  gun.get('#posts').get(curr.parent).once((prevStr) => {
    callback(prevStr);
  });
};

const validateSignature = async (json) => {
  if (!json.pubKey) {
    return true;
  }

  const withoutSig = { ...json, sig: undefined };
  const sig = Uint8Array.from(atob(json.sig), c => c.charCodeAt(0));
  const pubKey = await window.crypto.subtle.importKey("jwk", JSON.parse(json.pubKey), { name: "ECDSA", namedCurve: "P-256" }, true, ["verify"]);

  return await window.crypto.subtle.verify({ name: "ECDSA", hash: "SHA-256" }, pubKey, sig, utfBytes(JSON.stringify(withoutSig)).buffer);
};

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

app.ports.getComments.subscribe(async (post) => {
  const loadComments = (str, id) => {
    try {
      const json = JSON.parse(str);

      if (!validateSignature(json)) {
        return;
      }

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
  gun.get('#chunk_' + timestamp.toString()).map().once(async (str, id) => {
    try {
      const json = JSON.parse(str);

      if (!validateSignature(json)) {
        return;
      }

      const id = await SEA.work(str, null, null, { name: 'SHA-256' });
      gun.get('#posts').get(id).put(str);

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

const randomColor = () => {
  let r = Math.floor(Math.random()*256);
  let g = Math.floor(Math.random()*256);
  let b = Math.floor(Math.random()*256);
  return 'rgb(' + r + ',' + g + ',' + b + ')';
};

const captcha = () => {
  let showNum = [];
  let canvasWinth = 700;
  let canvasHeight = 146;
  let canvas = document.getElementById('captchaGen');
  let context = canvas.getContext('2d');
  canvas.width = canvasWinth;
  canvas.height = canvasHeight;
  let sCode = 'A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,0,1,2,3,4,5,6,7,8,9,!,@,#,$,%,^,&,*,(,)';
  let saCode = sCode.split(',');
  let saCodeLen = saCode.length;
  for (let i = 0; i <= 3; i++) {
    let sIndex = Math.floor(Math.random()*saCodeLen);
    let sDeg = (Math.random()*30*Math.PI) / 180;
    let cTxt = saCode[sIndex];
    showNum[i] = cTxt.toLowerCase();
    let x = 200 + i*90;
    let y = 90 + Math.random()*8;
    context.font = 'bold 90px arial';
    context.translate(x, y);
    context.rotate(sDeg);

    context.fillStyle = randomColor();
    context.fillText(cTxt, 0, 0);

    context.rotate(-sDeg);
    context.translate(-x, -y);
  }
  for (let i = 0; i <= 5; i++) {
    context.strokeStyle = randomColor();
    context.beginPath();
    context.moveTo(
      Math.random() * canvasWinth,
      Math.random() * canvasHeight
    );
    context.lineTo(
      Math.random() * canvasWinth,
      Math.random() * canvasHeight
    );
    context.stroke();
  }
  for (let i = 0; i < 6000; i++) {
    context.strokeStyle = randomColor();
    context.beginPath();
    let x = Math.random() * canvasWinth;
    let y = Math.random() * canvasHeight;
    context.moveTo(x,y);
    context.lineTo(x+1, y+1);
    context.stroke();
  }
  return showNum.join('');
};

const genCaptcha = async () => {
  const token = captcha();
  const canvas = document.getElementById('captchaGen');
  const parsed = { answer: token, data: canvas.toDataURL('image/jpeg') };
  app.ports.gotCaptcha.send(parsed);
};

app.ports.loadChunk.subscribe(loadChunk);
app.ports.genCaptcha.subscribe(genCaptcha);

app.ports.submitPost.subscribe(async (post) => {
  if (post.privKey !== null) {
    const sig = await sign(post.privKey, JSON.stringify(post.msg));
    post = { ...post.msg, sig: sig };
  }

  const data = JSON.stringify(post);
  const hash = await SEA.work(data, null, null, { name: 'SHA-256' });

  gun.get('#chunk_' + chunk(post.timestamp)).get(hash).put(data);
  gun.get('#posts').get(hash).put(data);
});

app.ports.submitComment.subscribe(async ([comment, rawParent]) => {
  gun.get('#posts').get(rawParent.id).once(async (parentStr, id) => {
    if (comment.privKey !== null) {
      const sig = await sign(comment.privKey, JSON.stringify(comment.msg));
      comment = { ...comment.msg, sig: sig };
    }

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
  });
}, 300);

setTimeout(() => {
  if (feed.scrollTop + feed.clientHeight >= feed.scrollHeight * 0.8) {
    app.ports.scrolledBottom.send(true);
  }
}, 1000);

genCaptcha();

// Load settings
const generateIdentity = async () => {
  const key = await window.crypto.subtle.generateKey({ name: "ECDSA", namedCurve: "P-256" }, true, ["sign", "verify"]);
  const pub = JSON.stringify(await window.crypto.subtle.exportKey("jwk", key.publicKey));
  const priv = JSON.stringify(await window.crypto.subtle.exportKey("jwk", key.privateKey));

  const iden = { "tripcode": "", "pubKey": pub, "privKey": priv };

  const oldSettings = getSettings();
  const settings = { ...oldSettings, identities: [...oldSettings.identities, iden]};
  window.localStorage.setItem("settings", JSON.stringify(settings));

  app.ports.loadedSettings.send(settings);
};

const getSettings = () => {
  let settings = {};

  try {
    const maybeSettings = window.localStorage.getItem("settings");

    if (maybeSettings) {
      settings = JSON.parse(maybeSettings);
    } else {
      settings = { "identities": [] };
    }
  } catch (e) {
    console.warn(e);
  }

  if (settings.identities.length == 0) {
    generateIdentity();
  }

  return settings;
};

app.ports.loadedSettings.send(getSettings());
app.ports.generateIdentity.subscribe(generateIdentity);

app.ports.modifiedSettings.subscribe(settings => {
  window.localStorage.setItem("settings", JSON.stringify(settings));
});

const sign = async (keyStr, msgStr) => {
  const msgBytes = utfBytes(msgStr).buffer;
  const jwk = JSON.parse(keyStr);
  const key = await window.crypto.subtle.importKey("jwk", jwk, { name: "ECDSA", namedCurve: "P-256" }, true, ["sign"]);
  const sig = await window.crypto.subtle.sign({ name: "ECDSA", hash: "SHA-256" }, key, msgBytes);
  return base64(sig);
};

window.gun = gun;
window.sea = SEA;

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
