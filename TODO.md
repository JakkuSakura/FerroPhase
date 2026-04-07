# TODO

以下为生产就绪方向的进度记录（文档已落地，工程落地仍待完成）：

- [ ] 定义并冻结“语义契约”与可执行规范（含多前端语义映射矩阵），任何发布前必须通过语义一致性基准套件。
  - 进度: 文档已完成（`docs/Language.md`, `docs/semantic/Matrix.md`, `docs/semantic/BaselineSuite.md`, `docs/QualityAssurance.md`）。
- [ ] 为每个前端建立“语义保持性”金标准与回归集，确保 IR/后端输出行为一致且可追溯。
  - 进度: 文档已完成（`docs/semantic/BaselineSuite.md`, `docs/QualityAssurance.md`）。
- [ ] 明确并文档化所有权/借用/生命周期与零成本抽象在 FerroPhase 中的约束范围，同时支持前端语义降级时的可解释规则。
  - 进度: 文档已完成（`docs/Language.md`, `docs/Design.md`, `docs/Pipeline.md`）。
- [ ] 建立发布门槛（必过项清单），发布前禁止绕过：语义一致性、稳定 API/ABI、关键基准测试、最小生态兼容性。
  - 进度: 文档已完成（`docs/QualityAssurance.md`, `docs/ReleaseArtifacts.md`, `docs/ProductionReadiness.md`）。
- [ ] 统一版本治理与兼容策略（语义版本 + 破坏性变更流程），禁止“隐式破坏”。
  - 进度: 文档已完成（`docs/VersionGovernance.md`）。
- [ ] 将标准库划分为稳定/实验/内部三层，稳定层引入兼容性承诺与弃用策略。
  - 进度: 文档已完成（`docs/Modules.md`, `docs/VersionGovernance.md`）。
- [ ] 建立可复现构建与签名发布流程，保证构建产物与提交可追溯。
  - 进度: 文档已完成（`docs/ReleaseArtifacts.md`, `docs/Compile.md`, `docs/BuildOptions.md`）。
- [ ] 构建最小可用 DX：LSP、格式化器、错误诊断质量门槛与“阶段化”调试支持。
  - 进度: 文档已完成（`docs/DeveloperExperience.md`）。
