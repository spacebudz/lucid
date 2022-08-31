import { LucidProvider } from '../context/LucidContext'
import '../styles/globals.css'
import type { AppProps } from 'next/app'

function MyApp({ Component, pageProps }: AppProps) {
  return (
    <>
      <LucidProvider>
        <Component {...pageProps} />
      </LucidProvider>
    </>
  );
}

export default MyApp
