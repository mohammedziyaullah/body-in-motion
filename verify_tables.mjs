import { spawn } from 'child_process';
import fs from 'fs';
import path from 'path';

const baseDir = 'C:/Users/moham/.gemini/antigravity/brain/a034e459-364a-4ecd-91e6-d9b4e4f529fc';
const chromePath = 'C:/Program Files/Google/Chrome/Application/chrome.exe';
const userDataDir = path.join(baseDir, 'chrome_temp_verify');

const chrome = spawn(chromePath, [
  '--headless=new',
  '--disable-gpu',
  '--remote-debugging-port=9224',
  `--user-data-dir=${userDataDir}`,
  '--window-size=1440,1200',
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
    } catch (e) {}
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
    await sleep(1500);
    const tabs = await waitForHttp('http://127.0.0.1:9224/json/list');
    const wsUrl = tabs[0].webSocketDebuggerUrl;
    const client = new CDPClient(wsUrl);
    await client.connect();

    await client.send('Page.enable');
    await client.send('Runtime.enable');
    await client.send('Emulation.setDeviceMetricsOverride', {
      width: 1440,
      height: 1200,
      deviceScaleFactor: 1,
      mobile: false
    });

    // 1. LIGHT MODE TESTS
    console.log('Navigating to Light Mode Data page...');
    await client.send('Page.navigate', { url: 'http://127.0.0.1:3838/?page=data&theme=light' });
    await sleep(3500);

    // Switch to Summary Statistics tab
    console.log('Clicking Summary Statistics tab in light mode...');
    await client.evaluate(`
      const tabs = Array.from(document.querySelectorAll('.nav-pills .nav-link'));
      const sumTab = tabs.find(t => t.textContent.includes('Summary Statistics'));
      if (sumTab) sumTab.click();
    `);
    await sleep(1500);
    await client.captureScreenshot(path.join(baseDir, 'page_summary_light.png'));
    console.log('Captured page_summary_light.png');

    // Switch to Data Table tab
    console.log('Clicking Data Table tab in light mode...');
    await client.evaluate(`
      const tabs = Array.from(document.querySelectorAll('.nav-pills .nav-link'));
      const dtTab = tabs.find(t => t.textContent.includes('Data Table'));
      if (dtTab) dtTab.click();
    `);
    await sleep(2500);
    await client.captureScreenshot(path.join(baseDir, 'page_datatable_light.png'));
    console.log('Captured page_datatable_light.png');

    // 2. DARK MODE TESTS
    console.log('Navigating to Dark Mode Data page...');
    await client.send('Page.navigate', { url: 'http://127.0.0.1:3838/?page=data&theme=dark' });
    await sleep(3500);

    // Summary Statistics tab in dark mode
    await client.evaluate(`
      const tabs = Array.from(document.querySelectorAll('.nav-pills .nav-link'));
      const sumTab = tabs.find(t => t.textContent.includes('Summary Statistics'));
      if (sumTab) sumTab.click();
    `);
    await sleep(1500);
    await client.captureScreenshot(path.join(baseDir, 'page_summary_dark.png'));
    console.log('Captured page_summary_dark.png');

    // Data Table tab in dark mode
    await client.evaluate(`
      const tabs = Array.from(document.querySelectorAll('.nav-pills .nav-link'));
      const dtTab = tabs.find(t => t.textContent.includes('Data Table'));
      if (dtTab) dtTab.click();
    `);
    await sleep(2500);
    await client.captureScreenshot(path.join(baseDir, 'page_datatable_dark.png'));
    console.log('Captured page_datatable_dark.png');

    client.close();
  } catch (e) {
    console.error('Error:', e);
  } finally {
    chrome.kill();
  }
}

run();
