require 'ch_lib'

class PostModel
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
    f_index.sort!

    # TODO: Paginate

    f_index.map! { |post_id|
      read_one(clan_id, thread_id, post_id)
    }
  end

  def self.read_one(clan_id, thread_id, post_id)
    rpc = Rails.application.get_rpc
    value = rpc.call.ch_post_api.read_one(clan_id, thread_id, post_id)
    return nil if value.nil?
    value['id'] = post_id
    value['created_at_d'] = DateTime.iso8601(value['created_at'])
    value['updated_at_d'] = DateTime.iso8601(value['updated_at'])
    ChLib.from_rpc(value)
    # value['title'] = value['title'].force_encoding('utf-8')
    # value['body'] = value['body'].force_encoding('utf-8')
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
