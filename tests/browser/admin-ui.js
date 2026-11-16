/* Private App Admin UI acceptance run. Start with NODE_PATH pointing at a Playwright install. */
const { chromium, firefox } = require('playwright');
const fs = require('fs'), os = require('os'), path = require('path'), http = require('http'), { spawn } = require('child_process');
const root = path.resolve(__dirname, '../..');
const temp = fs.mkdtempSync(path.join(os.tmpdir(), 'dualdag-wei37-'));
const app = path.join(temp, 'app'), home = path.join(temp, 'home'), project = path.join(temp, 'project'), bin = path.join(temp, 'bin'), urlFile = path.join(temp, 'url');
fs.cpSync(root,app,{recursive:true,filter:(src)=>!src.includes('/.git')&&!src.includes('/tests')});
for (const d of [home, project, path.join(project,'spec'), path.join(project,'programs'), bin]) fs.mkdirSync(d,{recursive:true});
fs.writeFileSync(path.join(project,'spec','note.txt'),'before\n'); fs.writeFileSync(path.join(project,'programs','main.R'),'# isolated\n');
const stub = path.join(bin,'claude');
fs.writeFileSync(stub, `#!/bin/sh
if [ "$1" = --version ]; then echo 'stub Claude 2.0'; exit 0; fi
if [ "$1" = auth ] && [ "$2" = status ]; then echo '{"loggedIn":true,"authMethod":"subscription"}'; exit 0; fi
prompt=$(cat)
sleep 2
printf 'after\n' >> "$ADMIN_PROJECT/spec/note.txt"
printf '/* stub app edit */\n' >> "$ADMIN_APP/www/style.css"
echo '{"type":"assistant","message":{"content":[{"type":"tool_use","name":"Edit","input":{"file_path":"spec/note.txt"}}]}}'
echo '{"type":"result","result":"Stub completed safely.","usage":{"input_tokens":12,"output_tokens":3}}'
`); fs.chmodSync(stub,0o755);
const port = 18371;
let server;
function waitUrl(child) { return new Promise((resolve,reject)=>{const start=Date.now(); const t=setInterval(()=>{if(fs.existsSync(urlFile)){clearInterval(t);resolve(fs.readFileSync(urlFile,'utf8').trim());}else if(child.exitCode!==null){clearInterval(t);reject(new Error('R server exited'));}else if(Date.now()-start>30000){clearInterval(t);reject(new Error('R server did not start'));}},100);}); }
async function runBrowser(engine,name,url) {
 const browser=await engine.launch({headless:true, ...(name==='chromium'?{executablePath:process.env.CHROME_BIN}: {})});
 const page=await browser.newPage(); const errors=[]; page.on('pageerror',e=>errors.push(String(e)));
 try {
  await page.goto(url); await page.getByRole('tab',{name:'Settings'}).click();
  const sw=page.locator('#admin-enabled'); await sw.waitFor(); if(await sw.isChecked())throw Error(name+': default enablement was not off');
  await page.getByRole('button',{name:'Admin agent'}).click();
  if(!await page.locator('.app-admin-overlay').getByRole('button',{name:'Send', exact:true}).isDisabled())throw Error(name+': disabled Admin allowed send');
  await page.getByRole('button',{name:'×'}).click(); await sw.check(); await page.waitForTimeout(200);
  await page.getByRole('tab',{name:/Spec & checks/}).click();
  await page.getByRole('button',{name:'Admin agent'}).click();
  const msg=page.locator('#app-admin-message'); await msg.fill('Please update the isolated project note.'); await page.locator('.app-admin-overlay').getByRole('button',{name:'Send',exact:true}).click();
  await page.getByText('Admin agent is working…').waitFor(); if(!await page.getByRole('button',{name:'Working…'}).isDisabled())throw Error(name+': Send was not disabled while working');
  await page.getByText('Stub completed safely.').waitFor({timeout:20000});
  await page.getByText('Files changed',{exact:true}).waitFor(); await page.getByText('spec/note.txt',{exact:true}).waitFor(); await page.getByText('www/style.css',{exact:true}).waitFor(); await page.getByText('Restart dualdag to apply app changes.').waitFor();
  page.once('dialog',d=>d.accept()); await page.getByRole('button',{name:'Undo',exact:true}).last().click();
  await page.waitForTimeout(300);
  if(fs.readFileSync(path.join(project,'spec','note.txt'),'utf8')!=='before\n') throw Error(name+': undo did not restore project file');
  await page.locator('.admin-snapshots summary').click(); await page.getByRole('button',{name:'Restore',exact:true}).first().waitFor();
  page.once('dialog',d=>d.accept()); await page.getByRole('button',{name:'Restore',exact:true}).first().evaluate(el=>el.click());
  await page.getByText(/Snapshot restored/).waitFor({timeout:20000});
  await page.locator('.app-admin-overlay').getByRole('button',{name:'Clear',exact:true}).evaluate(el=>el.click());
  await page.getByText('Start a conversation to edit this app and the open project.').waitFor();
  if(errors.length) throw Error(name+': page errors: '+errors.join('; '));
  console.log(name+': settings off/on, stub send/progress, changed-file card, undo, snapshot restore, clear, no page errors passed');
 } finally { await browser.close(); }
}
(async()=>{
 try {
  server=spawn(process.env.RSCRIPT||'Rscript',['run.R','--no-browser','--port='+port,'--project='+project],{cwd:app,env:{...process.env,HOME:home,DUALDAG_HOME:home,DUALDAG_NO_BROWSER:'1',DUALDAG_URL_FILE:urlFile,PATH:bin+path.delimiter+process.env.PATH,ADMIN_PROJECT:project,ADMIN_APP:app},stdio:'ignore'});
  const url=await waitUrl(server); await runBrowser(chromium,'chromium',url); await runBrowser(firefox,'firefox',url);
 } catch(e) { console.error(e.stack||e); process.exitCode=1; }
 finally { if(server){server.kill('SIGTERM'); await new Promise(r=>setTimeout(r,500));} fs.rmSync(temp,{recursive:true,force:true}); }
})();
