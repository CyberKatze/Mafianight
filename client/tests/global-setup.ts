import { spawn } from 'child_process';  

async function waitForService(url, timeout = 30000) {  
  const start = Date.now();  
  while (Date.now() - start < timeout) {  
    try {  
      const response = await fetch(url);  
      if (response.ok) {  
        return true;  
      }  
    } catch (e) {  
      // Ignore any errors as we are polling  
    }  
    await new Promise(r => setTimeout(r, 1000));  
  }  
  throw new Error(`Service at ${url} did not start within ${timeout / 1000} seconds`);  
}

export default async function globalSetup() {  
  // Start backend  
  spawn('make',[''], { cwd: '../', detached: false, stdio: ['ignore', 'pipe', 'pipe'] });
  console.log('Starting backend...');  
  spawn('make',['dev-back'], { cwd: '../', detached: true, stdio: ['ignore', 'pipe', 'pipe'] });

  // Wait for backend to be ready  
  console.log('Waiting for backend to be ready...');  
  await waitForService('http://localhost:3000');  // Adjust the URL to your backend health check endpoint

  // Start frontend  
  console.log('Starting frontend...');  
  spawn('make',['dev-front'], { cwd: '../', detached: true, stdio: ['ignore', 'pipe', 'pipe'] });

  // Wait for frontend to be ready  
  console.log('Waiting for frontend to be ready...');  
  await waitForService('http://localhost:3001');  // Adjust the URL to your frontend health check endpoint  

}  
