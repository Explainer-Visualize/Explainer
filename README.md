# Explainer Visualization Code

This repository contains R scripts for generating various visualizations used in the paper. Each script produces a high-quality PDF figure that can be included in the publication.

---

## Scripts

### 1. `ChestXray.R`
**Description:** Generates a multi-model performance comparison bar chart across 14 chest X-ray pathologies.

---

### 2. `ChestXray-loc.R`
**Description:** Creates localization accuracy line plots with error envelopes across IoU thresholds (0.3, 0.5, 0.7).

---

### 3. `MN-DS.R` 
**Description:** Comparison of document classification methods (Naive Bayes, Logistic Regression, Support Vector, Explainer) across precision, recall, and F1 metrics.   

---

### 4. `PDBch.R`
**Description:** Multi-panel bar chart comparing protein function prediction methods (Blast, FunFams, DeepFRI, HEAL, HEAL-Explainer) across three metrics (AUPR, Fmax, Smin) and three classes (MF, BP, CC).  

---

### 5. `FMA.R`
**Description:** Music genre classification comparison across precision, recall, and F1 for non-leaf and leaf genres.  

---

### 6. `FG3D.R`
**Description:** Accuracy visualization for 3D object classification models and their Explainer versions.  

---

### 7. `cifar-100.R`
**Description:** Comparison of base models (ResNet-50, Mixer-B, ViT-B) and explainer-enhanced versions on CIFAR-100.  

---

### 8. `centroid.R` 
**Description:** Analyzes accuracy vs. centroid number (1-8) for three model architectures.
Key Features. 

---

### 9. `ImageNet-1K.R`
**Description:** Top-1 accuracy of hierarchical classification methods (ResNet, Guided, HiMulConE, Explainer) across 12 hierarchical levels.  

---

### 10. `node.R`
**Description:** Dual-axis plot showing model accuracy (bars) and relative improvement (line) across different numbers of encompassed leaf nodes.  

---

### 11. `OOD-cifar10.R`
**Description:** OOD detection comparison on CIFAR-10 across multiple datasets (MNIST, SVHN, Textures, Places365, Mean).  

---

### 12. `OOD-ImageNet200.R`
**Description:** Compares OOD detection methods (GradNorm, Gram, ODIN, MDS, KNN, Explainer) on ImageNet-200 using AUROC and FPR@95.   

---

### 13. `user_study.R`
**Description:** Creates a stacked bar chart showing intelligibility scores across categories (Animal, Disease, Protein, Music).  

---


## Usage

### Install Required R Packages:
```r
install.packages(c("ggplot2", "scales", "patchwork", "gridExtra", "dplyr", "tidyr", "ggpattern"))
