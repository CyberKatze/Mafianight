import { spawn,execSync } from 'child_process';
import { promisify } from 'util';


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

function logOutput(childProcess, name) {
  childProcess.stdout.setEncoding('utf8');
  childProcess.stdout.on('data', (data) => {
    console.log(`[${name} stdout]:`, data.toString());
  });

  childProcess.stderr.setEncoding('utf8');
  childProcess.stderr.on('data', (data) => {
    console.error(`[${name} stderr]:`, data.toString());
  });

  childProcess.on('error', (error) => {
    console.error(`[${name} error]:`, error);
  });

  childProcess.on('exit', (code, signal) => {
    if (code !== null) {
      console.log(`[${name} exit]: Process exited with code ${code}`);
    } else {
      console.log(`[${name} exit]: Process exited due to signal ${signal}`);
    }
  });
}

export default async function globalSetup() {
  try {
    // Start backend build process
    console.log('Starting backend build process...');
    const make = spawn('make', { cwd: '../', stdio: ['ignore', 'pipe', 'pipe'] });
    logOutput(make, 'make');

    // Wait for the build process to complete
    await new Promise((resolve, reject) => {
      make.on('exit', (code) => {
        if (code === 0) {
          resolve(true);
        } else {
          reject(new Error(`Build process exited with code ${code}`));
        }
      });
    });

    // Health check for PostgreSQL using a psql command
async function waitForPostgres(timeout = 30000) {
  const start = Date.now();
  while (Date.now() - start < timeout) {
    try {
      var containerName = 'db';
      execSync(`pg_isready -h localhost -p 5432 `, { stdio: 'inherit' });
      return true;
    } catch (e) {
    }
    await new Promise(r => setTimeout(r, 1000));
  }
  throw new Error(`PostgreSQL service in container ${containerName} did not start within ${timeout / 1000} seconds`);
}

    // Start backend
    console.log('Starting db...');
    execSync('docker compose up -d db', { cwd: '../server', stdio: 'inherit' });

    // Wait for the db service to be ready (update this URL to match your DB health check endpoint)
    console.log('Waiting for db service to be ready...');
    await waitForPostgres();
    setTimeout(() => {}, 3000);

    console.log('Starting backend...');
    const back = spawn('stack', ['exec', 'haskell-web'], { cwd: '../server', stdio: ['ignore', 'pipe', 'pipe'] });
    // logOutput(back, 'backend');

    // Wait for backend to be ready
    console.log('Waiting for backend to be ready...');
    await waitForService('http://localhost:3000/roles');  // Adjust the URL to your backend health check endpoint
    // sleep for 2 seconds to make sure the backend is fully ready

    // Start frontend
    console.log('Starting frontend...');
    const front = spawn('make', ['dev-front'], { cwd: '../', stdio: ['ignore', 'pipe', 'pipe'] });
    // logOutput(front, 'frontend');

    // Wait for frontend to be ready
    console.log('Waiting for frontend to be ready...');
    await waitForService('http://localhost:3001');  // Adjust the URL to your frontend health check endpoint

  } catch (error) {
    console.error('Error during setup:', error);
    process.exit(1);
  }
}
