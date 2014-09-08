class Forum ## < ActiveRecord::Base
  # Список форумов для клана и его альянсов
  def self.list(clan_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_forum_api.list(clan_id) || []
  end
end
