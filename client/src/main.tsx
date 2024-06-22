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
      { path: 'rolecards/', element: <RoleCards /> }
    ],
  },
])
ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <RouterProvider router={router} />
  </React.StrictMode>,
)
