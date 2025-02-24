"use client";

import { motion } from "framer-motion";
import React from "react";
import PostContent from '@/contents';

const Content: React.FC<{ name: string }> = ({ name }) => {
  return (<motion.div
    initial={{ opacity: 0 }}
    animate={{ opacity: 1 }}
    exit={{ opacity: 0 }}
    transition={{ delay: 0.1 }}
  >
    <PostContent name={name} />
  </motion.div>);
}

export default Content;
