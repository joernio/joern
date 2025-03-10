package io.shiftleft.resolver.impl

// Notes about Swift dependency identification, declaration and reference.
// - Package.name: Not relevant for identifying an artifact.
//   The package name used for a dependency in a library using Package.swift has no correlation
//   to Package.name of the libraries Package.swift. Instead, the name is usually derived from
//   the package.url or package.path string and can be explicitly overridden with package.name.
// - Package.products.library.name: Part of the artifact identification.
// - Package.targets.target.name: Only used for referencing inside the same package.
//
// target.dependencies.byName(name: <name>): Is equivalent to target.dependencies.product(name: <name>, package: <name>)
//
// Dependency conflict resolution happens on the dependencies.package level and not on the target.dependencies level.
// Even if an extra package name is given via dependencies.package.name, it is not possible to include the same
// dependency in different version.
case class IdSwift()
