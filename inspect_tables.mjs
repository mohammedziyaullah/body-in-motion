import { spawn } from 'child_process';

async function main() {
  const chrome = spawn('C:/Program Files/Google/Chrome/Application/chrome.exe', [
    '--headless=new',
    '--remote-debugging-port=9223',
    '--user-data-dir=C:/Users/moham/.gemini/antigravity/brain/a034e459-364a-4ecd-91e6-d9b4e4f529fc/chrome_tmp_inspect',
    'about:blank'
  ]);
  await new Promise(r => setTimeout(r, 1500));
  const tabs = await (await fetch('http://127.0.0.1:9223/json/list')).json();
  const ws = new WebSocket(tabs[0].webSocketDebuggerUrl);
  await new Promise(r => ws.onopen = r);
  
  let id = 1;
  const callbacks = new Map();
  ws.onmessage = e => {
    const d = JSON.parse(e.data);
    if (d.id && callbacks.has(d.id)) {
      callbacks.get(d.id)(d.result);
      callbacks.delete(d.id);
    }
  };
  function send(method, params = {}) {
    return new Promise(res => {
      const curId = id++;
      callbacks.set(curId, res);
      ws.send(JSON.stringify({ id: curId, method, params }));
    });
  }

  await send('Page.enable');
  await send('Runtime.enable');
  await send('Page.navigate', { url: 'http://127.0.0.1:3838/?page=data&theme=light' });
  await new Promise(r => setTimeout(r, 3000));

  // Click on Summary Statistics tab
  await send('Runtime.evaluate', {
    expression: `
      const tab = Array.from(document.querySelectorAll('.nav-link')).find(el => el.textContent.includes('Summary Statistics'));
      if (tab) tab.click();
    `
  });
  await new Promise(r => setTimeout(r, 2000));

  const infoSummary = await send('Runtime.evaluate', {
    expression: `
      (function() {
        const sumTable = document.querySelector('#table_data_summary table');
        if (!sumTable) return { error: 'sumTable not found' };
        const th = sumTable.querySelector('thead th');
        const tr1 = sumTable.querySelector('tbody tr:nth-child(1)');
        const tr2 = sumTable.querySelector('tbody tr:nth-child(2)');
        const td2 = sumTable.querySelector('tbody tr:nth-child(2) td');
        return {
          classes: sumTable.className,
          thBg: th ? getComputedStyle(th).backgroundColor : 'none',
          thColor: th ? getComputedStyle(th).color : 'none',
          tr1Bg: tr1 ? getComputedStyle(tr1).backgroundColor : 'none',
          tr2Bg: tr2 ? getComputedStyle(tr2).backgroundColor : 'none',
          td2Bg: td2 ? getComputedStyle(td2).backgroundColor : 'none',
          td2Color: td2 ? getComputedStyle(td2).color : 'none'
        };
      })()
    `,
    returnByValue: true
  });
  console.log('Summary Table Light Info:', JSON.stringify(infoSummary.result.value, null, 2));

  // Click on Data Table tab
  await send('Runtime.evaluate', {
    expression: `
      const tab = Array.from(document.querySelectorAll('.nav-link')).find(el => el.textContent.includes('Data Table'));
      if (tab) tab.click();
    `
  });
  await new Promise(r => setTimeout(r, 2000));

  const infoDt = await send('Runtime.evaluate', {
    expression: `
      (function() {
        const dtTable = document.querySelector('#dt_data_records table');
        const wrapper = document.querySelector('#dt_data_records .dataTables_wrapper');
        if (!dtTable) return { error: 'dtTable not found' };
        const th = dtTable.querySelector('thead th');
        const tr1 = dtTable.querySelector('tbody tr:nth-child(1)');
        const tr2 = dtTable.querySelector('tbody tr:nth-child(2)');
        const td1 = dtTable.querySelector('tbody tr:nth-child(1) td');
        const paginateBtn = document.querySelector('.paginate_button');
        return {
          tableClasses: dtTable.className,
          wrapperBg: wrapper ? getComputedStyle(wrapper).backgroundColor : 'none',
          tableBg: getComputedStyle(dtTable).backgroundColor,
          thBg: th ? getComputedStyle(th).backgroundColor : 'none',
          thColor: th ? getComputedStyle(th).color : 'none',
          tr1Bg: tr1 ? getComputedStyle(tr1).backgroundColor : 'none',
          td1Bg: td1 ? getComputedStyle(td1).backgroundColor : 'none',
          td1Color: td1 ? getComputedStyle(td1).color : 'none',
          tr2Bg: tr2 ? getComputedStyle(tr2).backgroundColor : 'none',
          paginateBg: paginateBtn ? getComputedStyle(paginateBtn).backgroundColor : 'none',
          paginateColor: paginateBtn ? getComputedStyle(paginateBtn).color : 'none'
        };
      })()
    `,
    returnByValue: true
  });
  console.log('Data Table Light Info:', JSON.stringify(infoDt.result.value, null, 2));

  chrome.kill();
  process.exit(0);
}

main();
