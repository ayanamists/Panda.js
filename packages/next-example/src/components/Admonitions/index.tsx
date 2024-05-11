"use client";

import { Accordion, AccordionItem } from '@nextui-org/accordion';
import { useState } from 'react';

type AdmonitionsType = 'Note' | 'Warning' | 'other'

interface AdmonitionsProps {
  type: string;
  children: React.ReactNode;
}

export default function Admonitions({ type, children }: AdmonitionsProps) {
  const [selectedKeys, setSelectedKeys] = useState(new Set(["1"]));
  return (
    <Accordion className='not-prose'
      variant='splitted'
      selectedKeys={selectedKeys}
      onSelectionChange={(keys) => setSelectedKeys(keys as Set<string>)}
    >
      <AccordionItem title={type} key={1}>
        {children}
      </AccordionItem>
    </Accordion>
  );
}