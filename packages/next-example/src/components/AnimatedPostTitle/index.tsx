"use client";

import { motion } from "framer-motion";
import { getAnimateTitleId } from "@/utils";
import React from "react";

const Title: React.FC<{ id: string; heading: string }> = ({ id, heading }) => {
  return (
    <motion.div layoutId={getAnimateTitleId(id)}>
      <h1 className="text-2xl sm:text-3xl font-heading font-bold tracking-tight text-foreground/90 leading-tight">
        {heading}
      </h1>
    </motion.div>
  );
};

export default Title;
