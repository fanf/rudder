/*
*************************************************************************************
* Copyright 2013 Normation SAS
*************************************************************************************
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU Affero General Public License as
* published by the Free Software Foundation, either version 3 of the
* License, or (at your option) any later version.
*
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU Affero GPL v3, the copyright holders add the following
* Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU Affero GPL v3
* licence, when you create a Related Module, this Related Module is
* not considered as a part of the work and may be distributed under the
* license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/agpl.html>.
*
*************************************************************************************
*/

package com.normation.rudder.web.rest.node

import com.normation.rudder.web.services.rest.RestExtractorService
import com.normation.rudder.web.rest.node.service.NodeApiService1_0
import com.normation.rudder.web.rest.RestUtils._
import net.liftweb.http.Req
import net.liftweb.http.rest.RestHelper
import net.liftweb.common._
import net.liftweb.http.LiftResponse
import com.normation.rudder.web.rest.RestError
import com.normation.inventory.domain.NodeId

class NodeAPI1_0 (
  apiV1_0              : NodeApiService1_0
) extends RestHelper with NodeAPI with Loggable{


  val requestDispatch : PartialFunction[Req, () => Box[LiftResponse]] = {

    case Get(Nil, req) => apiV1_0.listAcceptedNodes(req)


    case Get("pending" :: Nil, req) => apiV1_0.listPendingNodes(req)

    case Get(id :: Nil, req) => apiV1_0.acceptedNodeDetails(req, NodeId(id))

    case Delete(id :: Nil, req) =>  apiV1_0.deleteNode(req, Seq(NodeId(id)))

    case Post("pending" :: Nil, req) => {
      apiV1_0.changeNodeStatus(req)
    }

  }
  serve( "api" / "1.0" / "nodes" prefix requestDispatch)

}
