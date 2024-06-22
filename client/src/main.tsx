import React from 'react'
import ReactDOM from 'react-dom/client'
import './index.css'
import Root from "./routes/root";
import { createBrowserRouter, RouterProvider } from 'react-router-dom'
import ErrorPage from './error-page.tsx';
import About from './routes/about.tsx';
import Loginform from './components/Loginform.tsx';
import Roles from './routes/roles.tsx';
import RoleCards from './routes/rolecards.tsx';
import Voting from './routes/voting.tsx';
import Gamesetup from './components/Gamesetup.tsx';


const handleStart = (numberOfPlayers: number, playerNames: string[], roles: { [key: string]: number }) => {
  console.log('Number of Players:', numberOfPlayers);
  console.log('Player Names:', playerNames);
  console.log('Roles:', roles);
  
};
const router = createBrowserRouter([
  {
    path: '/',
    element: <Root />,
    errorElement: <ErrorPage />,
    children: [
      {
        path: 'about/',
        element: <About />
      },
      {
        path: 'login/',
        element: <Loginform />
      },
      { path: 'roles/', element: <Roles /> },
      { path: 'rolecards/', element: <RoleCards /> },
      { path: 'voting/', element: <Voting /> },
      { path: 'game/', element: <Gamesetup onStart={handleStart}  />},
    ],
  },
])
ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <RouterProvider router={router} />
  </React.StrictMode>,
)
