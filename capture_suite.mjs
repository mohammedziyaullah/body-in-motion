import { spawn } from 'child_process';
import fs from 'fs';
import path from 'path';

const baseDir = 'C:/Users/moham/.gemini/antigravity/brain/a034e459-364a-4ecd-91e6-d9b4e4f529fc';
const chromePath = 'C:/Program Files/Google/Chrome/Application/chrome.exe';
const userDataDir = path.join(baseDir, 'chrome_temp_profile');

// Start Chrome with CDP enabled
const chrome = spawn(chromePath, [
  '--headless=new',
  '--disable-gpu',
  '--remote-debugging-port=9222',
  `--user-data-dir=${userDataDir}`,
  '--window-size=1440,1400',
  'about:blank'
]);

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function waitForHttp(url, attempts = 20) {
  for (let i = 0; i < attempts; i++) {
    try {
      const res = await fetch(url);
      if (res.ok) return await res.json();
    } catch (e) {
      // wait
    }
    await sleep(300);
  }
  throw new Error(`Failed to reach ${url}`);
}

class CDPClient {
  constructor(wsUrl) {
    this.wsUrl = wsUrl;
    this.ws = null;
    this.id = 1;
    this.callbacks = new Map();
  }

  async connect() {
    this.ws = new WebSocket(this.wsUrl);
    await new Promise((resolve, reject) => {
      this.ws.onopen = resolve;
      this.ws.onerror = reject;
    });

    this.ws.onmessage = (event) => {
      const data = JSON.parse(event.data);
      if (data.id && this.callbacks.has(data.id)) {
        const { resolve, reject } = this.callbacks.get(data.id);
        this.callbacks.delete(data.id);
        if (data.error) reject(data.error);
        else resolve(data.result);
      }
    };
  }

  send(method, params = {}) {
    const id = this.id++;
    return new Promise((resolve, reject) => {
      this.callbacks.set(id, { resolve, reject });
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }

  async evaluate(expr) {
    const res = await this.send('Runtime.evaluate', {
      expression: expr,
      returnByValue: true,
      awaitPromise: true
    });
    return res.result?.value;
  }

  async captureScreenshot(outPath) {
    const res = await this.send('Page.captureScreenshot', { format: 'png' });
    const buffer = Buffer.from(res.data, 'base64');
    fs.writeFileSync(outPath, buffer);
  }

  close() {
    if (this.ws) this.ws.close();
  }
}

async function run() {
  try {
    console.log('Waiting for Chrome CDP...');
    await sleep(1500);
    const tabs = await waitForHttp('http://127.0.0.1:9222/json/list');
    const wsUrl = tabs[0].webSocketDebuggerUrl;
    console.log('Connected to tab:', wsUrl);

    const client = new CDPClient(wsUrl);
    await client.connect();

    await client.send('Page.enable');
    await client.send('Runtime.enable');
    await client.send('Emulation.setDeviceMetricsOverride', {
      width: 1440,
      height: 1400,
      deviceScaleFactor: 1,
      mobile: false
    });

    console.log('Navigating to Shiny Data page...');
    await client.send('Page.navigate', { url: 'http://127.0.0.1:3838/?page=data' });
    await sleep(3500);

    const scenarios = [
      { name: 'anaerobic', condition: 'Anaerobic Exercise' },
      { name: 'stress', condition: 'Stress Session' },
      { name: 'rest', condition: 'Rest' },
      { name: 'aerobic', condition: 'Aerobic Exercise' },
      { name: 'all', condition: 'All Conditions' }
    ];

    for (const sc of scenarios) {
      console.log(`Setting condition to: ${sc.condition}`);
      await client.evaluate(`
        (function() {
          if (window.Shiny && window.Shiny.setInputValue) {
            if ("${sc.condition}" === "All Conditions") {
              Shiny.setInputValue('data_activity', 'All Conditions');
              Shiny.setInputValue('data_activity_checks', ['Rest', 'Aerobic Exercise', 'Anaerobic Exercise', 'Stress Session']);
            } else {
              Shiny.setInputValue('data_activity', "${sc.condition}");
              Shiny.setInputValue('data_activity_checks', ["${sc.condition}"]);
            }
          }
        })()
      `);

      // Wait for Plotly to render the lines
      console.log('Waiting for chart update...');
      let rendered = false;
      for (let i = 0; i < 20; i++) {
        await sleep(400);
        const hasPlots = await client.evaluate(`
          (function() {
            const plot = document.querySelector('#plot_data_timeseries .plotly');
            if (!plot) return false;
            const paths = plot.querySelectorAll('.scatterlayer path.js-line');
            return paths.length > 0;
          })()
        `);
        if (hasPlots) {
          rendered = true;
          break;
        }
      }
      console.log(`Render status for ${sc.name}: ${rendered}`);
      await sleep(1000); // Give time for transition/ticks to settle

      const fullPath = path.join(baseDir, `page_data_${sc.name}.png`);
      await client.captureScreenshot(fullPath);
      console.log(`Saved screenshot: ${fullPath}`);
    }

    client.close();
  } catch (err) {
    console.error('Error during execution:', err);
  } finally {
    chrome.kill();
    console.log('Chrome process terminated');
  }
}

run();
