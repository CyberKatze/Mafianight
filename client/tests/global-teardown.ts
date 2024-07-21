import { exec } from 'child_process';  
import { promisify } from 'util';  

const execAsync = promisify(exec);  

export default async function globalTeardown() {  
  try {  
    // Stop backend  
    console.log('Stopping backend...');  
    await execAsync('pkill -f "make dev-back"');  

    // Stop and clear database  
    console.log('Stopping database...');  
    await execAsync('docker compose down -v', { cwd: '../server' });  

    // Stop frontend  
    console.log('Stopping frontend...');  
    await execAsync('pkill -f "make dev-front"');  

  } catch (error) {  
    console.error('Error during teardown:', error.message);  
  }  
}
