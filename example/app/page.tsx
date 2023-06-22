import dynamic from 'next/dynamic';
import Image from 'next/image';

const Reflex = dynamic(() => import('../components/Reflex'), {
  ssr: false
})
const WithHaskell = dynamic(() => import('../components/Haskell'), {
  ssr: false
})

export default function Home() {
  return (
    <WithHaskell>
      <ul>
        <li><Reflex></Reflex></li>
        <li><Reflex></Reflex></li>
        <li><Reflex></Reflex></li>
      </ul>
    </WithHaskell>
  )
}
