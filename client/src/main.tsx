import React from 'react'
import ReactDOM from 'react-dom/client'
import './index.css'
import Root from "./routes/root";
import { createBrowserRouter, RouterProvider } from 'react-router-dom'
import ErrorPage from './error-page.tsx';
import Developer from './routes/develoeprs.tsx';

const router: any = createBrowserRouter([
  {
    path: '/',
    element: <Root />,
    errorElement: <ErrorPage />,
    children: [
      {
        path: 'developers/',
        element: <Developer />
      },
    ],
  },
  // {
  // path: '/about',
  // element: <About />,
  //   },
  //   {
  // path: '/contact',
  // element: <Contact />,
  //   },
  //   {
  // path: '/:catchAll',
  // element: <NotFound />,
  //   },
])
ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <RouterProvider router={router} />
  </React.StrictMode>,
)
