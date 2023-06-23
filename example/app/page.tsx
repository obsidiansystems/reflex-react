import dynamic from 'next/dynamic';
import Image from 'next/image';

const Reflex = dynamic(() => import('../components/Reflex'), {
  ssr: false
})
const WithHaskell = dynamic(() => import('../components/Haskell'), {
  ssr: false
})
const Test = dynamic(() => import('../components/Test'), {
  ssr: false
})

export default function Home() {
  return (
    <Test/>
  )
}
