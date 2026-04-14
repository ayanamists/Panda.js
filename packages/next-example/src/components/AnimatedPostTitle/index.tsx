"use client";

import { motion } from "framer-motion";
import { getAnimateTitleId } from "@/utils";
import React from "react";

const Title:
  React.FC<{
    id: string,
    heading: string,
  }> =
  ({ id, heading }) => {
    return (<motion.div
      layoutId={getAnimateTitleId(id)}
    >
      <h1 className="text-3xl font-bold mb-1 font-heading tracking-tight">{heading}</h1>
    </motion.div>);
  }

export default Title;
