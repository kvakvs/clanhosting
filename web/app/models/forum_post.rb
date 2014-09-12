class ForumPost
  # Поля:
  # string id (задаётся при чтении прямо здесь, не хранится в БД)
  # string title utf8
  # string body
  # string thread_id
  # int created_by

  # Список постов для треда
  def self.list(clan_id, thread_id)
    rpc = Rails.application.get_rpc
    f_index = rpc.call.ch_post_api.read_index(clan_id, thread_id) || []

    f_index.map! { |post_id|
      read_one(clan_id, thread_id, post_id)
    }
  end

  def self.read_one(clan_id, thread_id, post_id)
    rpc = Rails.application.get_rpc
    value = rpc.call.ch_post_api.read_one(clan_id, thread_id, post_id)
    return nil if value.nil?
    value['id'] = post_id
    value['body'] = value['body'].force_encoding('utf-8')
    value
  end

  def self.create(clan_id, thread_id, fields)
    rpc = Rails.application.get_rpc
    rpc.call.ch_post_api.add_post(clan_id, thread_id, fields)
  end

  def self.delete(clan_id, thread_id, post_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_post_api.delete_post(clan_id, thread_id, post_id)
  end
end
