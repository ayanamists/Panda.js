"use client";

import dynamic from 'next/dynamic';

const DynamicTopLoader = dynamic(() => import("@/components/TopLoader"), {
  ssr: false
});

export default DynamicTopLoader;
