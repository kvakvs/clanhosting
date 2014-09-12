class NewsfeedItem
  def self.all_for_clan(clan_id)
    rpc = Rails.application.get_rpc
    news = rpc.call.ch_newsfeed_api.read_index(clan_id) || []
    news.map! { |item_id|
      read_one(clan_id, item_id)
    }
  end

  def self.read_one(clan_id, item_id)
    rpc = Rails.application.get_rpc
    value = rpc.call.ch_newsfeed_api.read_one(clan_id, item_id)
    value['id'] = item_id
    value
  end

  def self.delete(clan_id, id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_newsfeed_api.delete_post(clan_id, id)
  end

  def self.create(clan_id, fields)
    rpc = Rails.application.get_rpc
    rpc.call.ch_newsfeed_api.add_post(clan_id, fields)
  end
end
