import { execSync } from 'child_process';  

export default function globalTeardown() {  
  try {  
    // Stop backend  
    console.log('Stopping backend...');  
    execSync(`lsof -t -i:3000 | xargs kill -9`, { stdio: 'pipe' });  
    // Stop and clear database
    console.log('Stopping database...');  
    execSync('docker compose down -v', { cwd: '../server', stdio: 'inherit' });  

    // Stop frontend  
    console.log('Stopping frontend...');  
    execSync(`lsof -t -i:3001 | xargs kill -9`, { stdio: 'pipe' });  

    console.log('Teardown completed.');  
  } catch (error) {  
    console.error('Error during teardown:', error.message);  
  }  
}
