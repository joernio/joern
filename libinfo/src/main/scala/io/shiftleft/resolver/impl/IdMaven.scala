package io.shiftleft.resolver.impl

import io.shiftleft.resolver.api.Id

case class IdMaven(groupId: String, artifactId: String) extends Id {
  override val value: String = s"$groupId:$artifactId"

  override def toString: String = {
    value
  }
}

