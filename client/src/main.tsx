import React from 'react';
import ReactDOM from 'react-dom/client';
import { createBrowserRouter, RouterProvider } from 'react-router-dom';
import ProtectedRoute from './components/Protectedroutes.tsx';
import ErrorPage from './error-page.tsx';
import './index.css';
import About from './routes/about.tsx';
import Gamesetup from './routes/Gamesetup.tsx';
import Home from './routes/home.tsx';
import Loginform from './routes/Loginform.tsx';
import LoginRequired from './routes/loginRequired.tsx';
import RoleCards from './routes/rolecards.tsx';
import Roles from './routes/roles.tsx';
import Root from "./routes/root";
import Signup from './routes/Signup.tsx';
import Voting from './routes/voting.tsx';

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
      { path: '/', element: <Home /> },
      {
        path: 'about/',
        element: <About />
      },
      {
        path: 'login/',
        element: <Loginform />
      },
      { path: 'signup/', element: <Signup />},
      { path: 'roles/', element: <Roles /> },
      { path: 'rolecards/', element: <RoleCards /> },
      { path: 'voting/', element: <Voting /> },
      { path: 'gamesetup/', element:  <ProtectedRoute>
     <Gamesetup onStart={handleStart}/>
    </ProtectedRoute>},
      { path: 'loginrequired/', element:  <LoginRequired/> },
    ],
  },
])
ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <RouterProvider router={router} />
  </React.StrictMode>,
)
